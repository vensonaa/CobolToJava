from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
import asyncio
import json
import time
from typing import Dict, Any, List, Optional
import uvicorn
from pydantic import BaseModel
import re
import subprocess
import tempfile
import os

from cobol_analyzer import COBOLAnalyzer
from java_optimizer import JavaOptimizer
from code_validator import CodeValidator
from utils import logger

app = FastAPI(title="COBOL to Java MCP Server", version="1.0.0")

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize components
cobol_analyzer = COBOLAnalyzer()
java_optimizer = JavaOptimizer()
code_validator = CodeValidator()

class MCPRequest(BaseModel):
    method: str
    params: Dict[str, Any]

class MCPResponse(BaseModel):
    result: Dict[str, Any]
    error: Optional[str] = None

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "services": {
            "cobol_analyzer": cobol_analyzer.is_ready(),
            "java_optimizer": java_optimizer.is_ready(),
            "code_validator": code_validator.is_ready()
        }
    }

@app.post("/mcp/call")
async def mcp_call(request: MCPRequest):
    """Handle MCP method calls"""
    try:
        method = request.method
        params = request.params
        
        if method == "analyze_cobol":
            result = await analyze_cobol_handler(params)
        elif method == "optimize_java":
            result = await optimize_java_handler(params)
        elif method == "validate_conversion":
            result = await validate_conversion_handler(params)
        elif method == "get_conversion_suggestions":
            result = await get_conversion_suggestions_handler(params)
        elif method == "compile_java":
            result = await compile_java_handler(params)
        elif method == "run_tests":
            result = await run_tests_handler(params)
        elif method == "generate_documentation":
            result = await generate_documentation_handler(params)
        elif method == "estimate_complexity":
            result = await estimate_complexity_handler(params)
        elif method == "get_best_practices":
            result = await get_best_practices_handler(params)
        else:
            raise HTTPException(status_code=400, detail=f"Unknown method: {method}")
        
        return MCPResponse(result=result)
        
    except Exception as e:
        logger.error(f"MCP call error: {e}")
        return MCPResponse(result={}, error=str(e))

@app.post("/mcp/stream")
async def mcp_stream(request: MCPRequest):
    """Handle streaming MCP method calls"""
    async def generate():
        try:
            method = request.method
            params = request.params
            
            if method == "stream_analysis":
                async for chunk in stream_analysis_handler(params):
                    yield f"data: {json.dumps(chunk)}\n\n"
            else:
                yield f"data: {json.dumps({'error': f'Unknown streaming method: {method}'})}\n\n"
                
        except Exception as e:
            yield f"data: {json.dumps({'error': str(e)})}\n\n"
    
    return StreamingResponse(generate(), media_type="text/plain")

# MCP Method Handlers

async def analyze_cobol_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Analyze COBOL code"""
    code = params.get("code", "")
    analysis_type = params.get("analysis_type", "general")
    
    if not code.strip():
        return {"error": "Empty COBOL code provided"}
    
    try:
        analysis = await cobol_analyzer.analyze(code, analysis_type)
        return {
            "analysis": analysis,
            "analysis_type": analysis_type,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"COBOL analysis error: {e}")
        return {"error": str(e)}

async def optimize_java_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Optimize Java code"""
    java_code = params.get("java_code", "")
    original_cobol = params.get("original_cobol", "")
    
    if not java_code.strip():
        return {"error": "Empty Java code provided"}
    
    try:
        optimized_code = await java_optimizer.optimize(java_code, original_cobol)
        return {
            "optimized_code": optimized_code,
            "optimization_metrics": java_optimizer.get_metrics(),
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Java optimization error: {e}")
        return {"error": str(e)}

async def validate_conversion_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Validate COBOL to Java conversion"""
    cobol_code = params.get("cobol_code", "")
    java_code = params.get("java_code", "")
    
    if not cobol_code.strip() or not java_code.strip():
        return {"error": "Both COBOL and Java code required"}
    
    try:
        validation = await code_validator.validate_conversion(cobol_code, java_code)
        return {
            "validation": validation,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Validation error: {e}")
        return {"error": str(e)}

async def get_conversion_suggestions_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Get conversion suggestions for COBOL code"""
    cobol_code = params.get("cobol_code", "")
    
    if not cobol_code.strip():
        return {"error": "Empty COBOL code provided"}
    
    try:
        suggestions = await cobol_analyzer.get_conversion_suggestions(cobol_code)
        return {
            "suggestions": suggestions,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Suggestions error: {e}")
        return {"error": str(e)}

async def compile_java_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Compile Java code to check for errors"""
    java_code = params.get("java_code", "")
    
    if not java_code.strip():
        return {"error": "Empty Java code provided"}
    
    try:
        compilation_result = await code_validator.compile_java(java_code)
        return {
            "compilation": compilation_result,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Compilation error: {e}")
        return {"error": str(e)}

async def run_tests_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Run tests on converted Java code"""
    java_code = params.get("java_code", "")
    test_cases = params.get("test_cases", [])
    
    if not java_code.strip():
        return {"error": "Empty Java code provided"}
    
    try:
        test_results = await code_validator.run_tests(java_code, test_cases)
        return {
            "test_results": test_results,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Test execution error: {e}")
        return {"error": str(e)}

async def generate_documentation_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Generate documentation for the conversion"""
    cobol_code = params.get("cobol_code", "")
    java_code = params.get("java_code", "")
    
    if not cobol_code.strip() or not java_code.strip():
        return {"error": "Both COBOL and Java code required"}
    
    try:
        documentation = await cobol_analyzer.generate_documentation(cobol_code, java_code)
        return {
            "documentation": documentation,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Documentation generation error: {e}")
        return {"error": str(e)}

async def estimate_complexity_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Estimate conversion complexity"""
    cobol_code = params.get("cobol_code", "")
    
    if not cobol_code.strip():
        return {"error": "Empty COBOL code provided"}
    
    try:
        complexity = await cobol_analyzer.estimate_complexity(cobol_code)
        return {
            "complexity": complexity,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Complexity estimation error: {e}")
        return {"error": str(e)}

async def get_best_practices_handler(params: Dict[str, Any]) -> Dict[str, Any]:
    """Get best practices for the conversion"""
    context = params.get("context", {})
    
    try:
        best_practices = await cobol_analyzer.get_best_practices(context)
        return {
            "best_practices": best_practices,
            "timestamp": time.time()
        }
    except Exception as e:
        logger.error(f"Best practices error: {e}")
        return {"error": str(e)}

async def stream_analysis_handler(params: Dict[str, Any]):
    """Stream analysis results"""
    code = params.get("code", "")
    analysis_type = params.get("analysis_type", "comprehensive")
    
    if not code.strip():
        yield {"error": "Empty code provided"}
        return
    
    try:
        # Stream analysis steps
        yield {"step": "starting", "message": "Beginning analysis..."}
        
        # Step 1: Basic validation
        yield {"step": "validation", "message": "Validating code structure..."}
        validation = await cobol_analyzer.validate_code(code)
        yield {"step": "validation_complete", "data": validation}
        
        # Step 2: Complexity analysis
        yield {"step": "complexity", "message": "Analyzing complexity..."}
        complexity = await cobol_analyzer.analyze_complexity(code)
        yield {"step": "complexity_complete", "data": complexity}
        
        # Step 3: Structure analysis
        yield {"step": "structure", "message": "Analyzing code structure..."}
        structure = await cobol_analyzer.analyze_structure(code)
        yield {"step": "structure_complete", "data": structure}
        
        # Step 4: Conversion planning
        yield {"step": "planning", "message": "Creating conversion plan..."}
        plan = await cobol_analyzer.create_conversion_plan(code)
        yield {"step": "planning_complete", "data": plan}
        
        # Final result
        yield {
            "step": "complete",
            "message": "Analysis completed",
            "summary": {
                "validation": validation,
                "complexity": complexity,
                "structure": structure,
                "plan": plan
            }
        }
        
    except Exception as e:
        logger.error(f"Stream analysis error: {e}")
        yield {"error": str(e)}

# WebSocket endpoint for real-time communication
@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()
    
    try:
        while True:
            data = await websocket.receive_text()
            message = json.loads(data)
            
            # Handle WebSocket messages
            if message.get("type") == "analyze":
                result = await analyze_cobol_handler(message.get("params", {}))
                await websocket.send_text(json.dumps(result))
            elif message.get("type") == "optimize":
                result = await optimize_java_handler(message.get("params", {}))
                await websocket.send_text(json.dumps(result))
            else:
                await websocket.send_text(json.dumps({"error": "Unknown message type"}))
                
    except WebSocketDisconnect:
        logger.info("WebSocket client disconnected")
    except Exception as e:
        logger.error(f"WebSocket error: {e}")

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8001)
