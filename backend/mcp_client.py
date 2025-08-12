import asyncio
import json
import httpx
from typing import Dict, Any, Optional
from utils import logger

class MCPClient:
    def __init__(self, server_url: str = "http://localhost:8001"):
        self.server_url = server_url
        self.client = None
        self.connected = False
        
    async def connect(self):
        """Connect to the MCP server"""
        try:
            self.client = httpx.AsyncClient(timeout=10.0)  # Reduced timeout
            # Test connection
            response = await self.client.get(f"{self.server_url}/health")
            if response.status_code == 200:
                self.connected = True
                logger.info("MCP client connected successfully")
            else:
                logger.warning(f"MCP server health check failed: {response.status_code}")
                self.connected = False
        except httpx.ConnectError:
            logger.warning("MCP server not available at " + self.server_url)
            self.connected = False
        except Exception as e:
            logger.warning(f"Failed to connect to MCP server: {e}")
            self.connected = False
    
    async def disconnect(self):
        """Disconnect from the MCP server"""
        if self.client:
            await self.client.aclose()
            self.connected = False
            logger.info("MCP client disconnected")
    
    def is_connected(self) -> bool:
        """Check if connected to MCP server"""
        return self.connected
    
    async def call_method(self, method: str, params: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Call a method on the MCP server"""
        if not self.connected or not self.client:
            logger.warning("MCP client not connected")
            return None
        
        try:
            request_data = {
                "method": method,
                "params": params
            }
            
            response = await self.client.post(
                f"{self.server_url}/mcp/call",
                json=request_data,
                headers={"Content-Type": "application/json"}
            )
            
            if response.status_code == 200:
                result = response.json()
                return result.get("result")
            else:
                logger.error(f"MCP call failed: {response.status_code} - {response.text}")
                return None
                
        except Exception as e:
            logger.error(f"Error calling MCP method {method}: {e}")
            return None
    
    async def analyze_cobol(self, code: str, analysis_type: str = "general") -> Optional[Dict[str, Any]]:
        """Analyze COBOL code using MCP server"""
        return await self.call_method("analyze_cobol", {
            "code": code,
            "analysis_type": analysis_type
        })
    
    async def optimize_java(self, java_code: str, original_cobol: str) -> Optional[str]:
        """Optimize Java code using MCP server"""
        result = await self.call_method("optimize_java", {
            "java_code": java_code,
            "original_cobol": original_cobol
        })
        return result.get("optimized_code") if result else None
    
    async def validate_conversion(self, cobol_code: str, java_code: str) -> Optional[Dict[str, Any]]:
        """Validate COBOL to Java conversion"""
        return await self.call_method("validate_conversion", {
            "cobol_code": cobol_code,
            "java_code": java_code
        })
    
    async def get_conversion_suggestions(self, cobol_code: str) -> Optional[Dict[str, Any]]:
        """Get conversion suggestions for COBOL code"""
        return await self.call_method("get_conversion_suggestions", {
            "cobol_code": cobol_code
        })
    
    async def compile_java(self, java_code: str) -> Optional[Dict[str, Any]]:
        """Compile Java code to check for errors"""
        return await self.call_method("compile_java", {
            "java_code": java_code
        })
    
    async def run_tests(self, java_code: str, test_cases: list) -> Optional[Dict[str, Any]]:
        """Run tests on converted Java code"""
        return await self.call_method("run_tests", {
            "java_code": java_code,
            "test_cases": test_cases
        })
    
    async def generate_documentation(self, cobol_code: str, java_code: str) -> Optional[str]:
        """Generate documentation for the conversion"""
        result = await self.call_method("generate_documentation", {
            "cobol_code": cobol_code,
            "java_code": java_code
        })
        return result.get("documentation") if result else None
    
    async def estimate_complexity(self, cobol_code: str) -> Optional[Dict[str, Any]]:
        """Estimate conversion complexity"""
        return await self.call_method("estimate_complexity", {
            "cobol_code": cobol_code
        })
    
    async def get_best_practices(self, conversion_context: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Get best practices for the conversion"""
        return await self.call_method("get_best_practices", {
            "context": conversion_context
        })
    
    async def stream_analysis(self, cobol_code: str, analysis_type: str = "comprehensive"):
        """Stream analysis results from MCP server"""
        if not self.connected or not self.client:
            logger.warning("MCP client not connected")
            return
        
        try:
            async with self.client.stream(
                "POST",
                f"{self.server_url}/mcp/stream",
                json={
                    "method": "stream_analysis",
                    "params": {
                        "code": cobol_code,
                        "analysis_type": analysis_type
                    }
                }
            ) as response:
                async for chunk in response.aiter_text():
                    if chunk.strip():
                        try:
                            data = json.loads(chunk)
                            yield data
                        except json.JSONDecodeError:
                            yield {"raw_data": chunk}
                            
        except Exception as e:
            logger.error(f"Error in stream analysis: {e}")
            yield {"error": str(e)}
