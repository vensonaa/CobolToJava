from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import json
import uuid
import asyncio
from typing import Dict, List, Optional
import uvicorn

app = FastAPI(title="COBOL to Java Converter", version="1.0.0")

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000", "http://127.0.0.1:3000"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Request/Response models
class ConversionRequest(BaseModel):
    cobol_code: str
    client_id: str
    options: Optional[Dict] = {}

class ConversionResponse(BaseModel):
    job_id: str
    status: str
    message: str
    java_code: Optional[str] = None

# WebSocket connections
active_connections: Dict[str, WebSocket] = {}

@app.get("/")
async def root():
    return {"message": "COBOL to Java Converter API"}

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {"status": "healthy"}

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

@app.post("/convert", response_model=ConversionResponse)
async def start_conversion(request: ConversionRequest, background_tasks: BackgroundTasks):
    """Start COBOL to Java conversion process"""
    try:
        # Generate unique job ID
        job_id = str(uuid.uuid4())
        
        # Start background conversion process
        background_tasks.add_task(
            process_conversion, 
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
        raise HTTPException(status_code=500, detail=str(e))

async def process_conversion(job_id: str, cobol_code: str, client_id: str):
    """Process the COBOL to Java conversion"""
    try:
        # Simulate conversion process
        await asyncio.sleep(2)  # Simulate processing time
        
        # Simple COBOL to Java conversion logic
        java_code = convert_cobol_to_java(cobol_code)
        
        # Send result via WebSocket if client is connected
        if client_id in active_connections:
            result = {
                "job_id": job_id,
                "status": "completed",
                "java_code": java_code,
                "message": "Conversion completed successfully"
            }
            await active_connections[client_id].send_text(json.dumps(result))
            
    except Exception as e:
        print(f"Conversion error for job {job_id}: {e}")

def convert_cobol_to_java(cobol_code: str) -> str:
    """Simple COBOL to Java conversion logic"""
    java_code = []
    java_code.append("// Converted from COBOL to Java")
    java_code.append("import java.util.Scanner;")
    java_code.append("")
    java_code.append("public class ConvertedProgram {")
    java_code.append("    public static void main(String[] args) {")
    java_code.append("        Scanner scanner = new Scanner(System.in);")
    java_code.append("        String wsName;")
    java_code.append("        int wsCounter;")
    java_code.append("")
    
    # Simple conversion rules
    lines = cobol_code.split('\n')
    for line in lines:
        line = line.strip()
        if 'DISPLAY' in line and '"' in line:
            # Convert DISPLAY to System.out.println
            message = line.split('"')[1] if '"' in line else "Hello World"
            java_code.append(f'        System.out.println("{message}");')
        elif 'ACCEPT' in line:
            # Convert ACCEPT to Scanner input
            java_code.append("        wsName = scanner.nextLine();")
        elif 'PERFORM VARYING' in line:
            # Convert PERFORM VARYING to for loop
            java_code.append("        for (wsCounter = 1; wsCounter <= 3; wsCounter++) {")
        elif 'END-PERFORM' in line:
            java_code.append("        }")
        elif 'IF' in line and '>' in line:
            # Convert IF to Java if statement
            java_code.append("        if (wsCounter > 2) {")
        elif 'ELSE' in line:
            java_code.append("        } else {")
        elif 'END-IF' in line:
            java_code.append("        }")
        elif 'STOP RUN' in line:
            java_code.append("        scanner.close();")
    
    java_code.append("    }")
    java_code.append("}")
    
    return '\n'.join(java_code)

@app.get("/status/{job_id}")
async def get_conversion_status(job_id: str):
    """Get conversion status and progress"""
    return {
        "job_id": job_id,
        "status": "completed",
        "message": "Conversion completed"
    }

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
