from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
from contextlib import asynccontextmanager
import asyncio
import json
import uuid
from typing import Dict, List, Optional, Any
import uvicorn

from models import ConversionRequest, ConversionResponse, FeedbackRequest
from database import DatabaseManager
from workflow import ConversionWorkflow
from mcp_client import MCPClient
from utils import logger

# Global instances
db_manager = DatabaseManager()
workflow = ConversionWorkflow()
mcp_client = MCPClient()

# WebSocket connections
active_connections: Dict[str, WebSocket] = {}

@asynccontextmanager
async def lifespan(app: FastAPI):
    """Lifespan events for startup and shutdown"""
    # Startup
    await db_manager.initialize()
    
    # Try to connect to MCP server (optional)
    try:
        await mcp_client.connect()
        if mcp_client.is_connected():
            logger.info("MCP server connected successfully")
        else:
            logger.info("MCP server not available - continuing without enhanced features")
    except Exception as e:
        logger.info("MCP server connection failed - continuing without enhanced features")
    
    logger.info("Application started successfully")
    
    yield
    
    # Shutdown
    await mcp_client.disconnect()
    logger.info("Application shutdown complete")

app = FastAPI(
    title="COBOL to Java Converter", 
    version="1.0.0",
    lifespan=lifespan
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000", "http://127.0.0.1:3000"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str):
    """WebSocket endpoint for real-time updates"""
    await websocket.accept()
    active_connections[client_id] = websocket
    
    try:
        while True:
            data = await websocket.receive_text()
            # Handle incoming messages if needed
            await websocket.send_text(f"Message received: {data}")
    except WebSocketDisconnect:
        if client_id in active_connections:
            del active_connections[client_id]

async def send_websocket_update(client_id: str, message: dict):
    """Send update to specific WebSocket client"""
    if client_id in active_connections:
        try:
            await active_connections[client_id].send_text(json.dumps(message))
        except Exception as e:
            logger.error(f"Failed to send WebSocket update: {e}")

@app.get("/")
async def root():
    """Health check endpoint"""
    return {"message": "COBOL to Java Converter API", "status": "running"}

@app.options("/convert")
async def convert_options():
    """Handle CORS preflight requests for /convert endpoint"""
    return {"message": "OK"}

@app.post("/convert", response_model=ConversionResponse)
async def start_conversion(request: ConversionRequest, background_tasks: BackgroundTasks):
    """Start COBOL to Java conversion process"""
    try:
        # Generate unique job ID
        job_id = str(uuid.uuid4())
        
        # Save initial request to database
        await db_manager.save_conversion_request(job_id, request)
        
        # Start background conversion process
        background_tasks.add_task(
            run_conversion_workflow, 
            job_id, 
            request.cobol_code, 
            request.client_id
        )
        
        return ConversionResponse(
            job_id=job_id,
            status="started",
            message="Conversion process initiated"
        )
        
    except Exception as e:
        logger.error(f"Error starting conversion: {e}")
        raise HTTPException(status_code=500, detail=str(e))

async def run_conversion_workflow(job_id: str, cobol_code: str, client_id: str):
    """Run the complete conversion workflow"""
    try:
        # Update status
        await db_manager.update_status(job_id, "planning")
        await send_websocket_update(client_id, {
            "job_id": job_id,
            "status": "planning",
            "message": "Analyzing COBOL code and creating conversion plan..."
        })
        
        # Try to execute workflow
        try:
            result = await workflow.execute(cobol_code, job_id, client_id)
        except Exception as workflow_error:
            logger.warning(f"Workflow failed, using fallback conversion: {workflow_error}")
            # Use fallback conversion
            result = await fallback_conversion(cobol_code, job_id, client_id)
        
        # Save final result
        await db_manager.save_conversion_result(job_id, result)
        
        # Send completion notification
        await send_websocket_update(client_id, {
            "job_id": job_id,
            "status": "completed",
            "result": result
        })
        
    except Exception as e:
        logger.error(f"Conversion error for job {job_id}: {e}")
        await db_manager.update_status(job_id, "failed")
        await send_websocket_update(client_id, {
            "job_id": job_id,
            "status": "failed",
            "error": str(e)
        })

async def fallback_conversion(cobol_code: str, job_id: str, client_id: str) -> Dict[str, Any]:
    """Fallback conversion method that doesn't rely on external APIs"""
    try:
        await db_manager.update_status(job_id, "converting")
        await send_websocket_update(client_id, {
            "job_id": job_id,
            "status": "converting",
            "message": "Converting COBOL to Java using fallback method..."
        })
        
        # Simple COBOL to Java conversion logic
        java_code = convert_cobol_to_java_simple(cobol_code)
        
        await db_manager.update_status(job_id, "completed")
        
        return {
            "job_id": job_id,
            "status": "completed",
            "cobol_code": cobol_code,
            "java_code": java_code,
            "conversion_plan": {"method": "fallback_simple"},
            "review_comments": ["Converted using fallback method"],
            "fixes_applied": [],
            "verification_results": {"method": "fallback"},
            "execution_time": 0.0,
            "error_message": None
        }
        
    except Exception as e:
        logger.error(f"Fallback conversion error: {e}")
        raise

def convert_cobol_to_java_simple(cobol_code: str) -> str:
    """Use the enhanced workflow conversion method"""
    # Use the workflow's enhanced conversion method
    return workflow._simple_cobol_to_java(cobol_code)

@app.get("/status/{job_id}")
async def get_conversion_status(job_id: str):
    """Get conversion status and progress"""
    try:
        status = await db_manager.get_status(job_id)
        return status
    except Exception as e:
        raise HTTPException(status_code=404, detail="Job not found")

@app.post("/feedback")
async def submit_feedback(feedback: FeedbackRequest):
    """Submit human feedback for conversion process"""
    try:
        await db_manager.save_feedback(feedback.job_id, feedback)
        
        # If feedback requires rework, trigger specific workflow node
        if feedback.requires_rework:
            await workflow.handle_feedback(feedback)
        
        return {"message": "Feedback received and processed"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/history")
async def get_conversion_history(limit: int = 50, offset: int = 0):
    """Get conversion history"""
    try:
        history = await db_manager.get_history(limit, offset)
        return history
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/batch")
async def batch_conversion(requests: List[ConversionRequest]):
    """Process multiple COBOL files in batch"""
    try:
        job_ids = []
        for request in requests:
            job_id = str(uuid.uuid4())
            await db_manager.save_conversion_request(job_id, request)
            job_ids.append(job_id)
        
        # Start batch processing
        asyncio.create_task(process_batch(job_ids, requests))
        
        return {"job_ids": job_ids, "message": "Batch processing started"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

async def process_batch(job_ids: List[str], requests: List[ConversionRequest]):
    """Process batch of conversions with parallel execution"""
    tasks = []
    for job_id, request in zip(job_ids, requests):
        task = run_conversion_workflow(job_id, request.cobol_code, request.client_id)
        tasks.append(task)
    
    # Execute all conversions in parallel
    await asyncio.gather(*tasks, return_exceptions=True)

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {"status": "healthy", "mcp_connected": mcp_client.is_connected()}

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
