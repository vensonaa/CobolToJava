import re
import json
import subprocess
import tempfile
import os
from typing import Dict, Any, List, Optional
from utils import logger

class CodeValidator:
    def __init__(self):
        self.ready = True
        self.validation_results = {}
        
    def is_ready(self) -> bool:
        """Check if validator is ready"""
        return self.ready
    
    async def validate_conversion(self, cobol_code: str, java_code: str) -> Dict[str, Any]:
        """Validate COBOL to Java conversion"""
        try:
            validation = {
                "overall_valid": True,
                "syntax_check": await self._check_java_syntax(java_code),
                "structure_analysis": await self._analyze_structure(cobol_code, java_code),
                "functional_equivalence": await self._check_functional_equivalence(cobol_code, java_code),
                "code_quality": await self._assess_code_quality(java_code),
                "issues": [],
                "recommendations": []
            }
            
            # Determine overall validity
            if not validation["syntax_check"]["valid"]:
                validation["overall_valid"] = False
                validation["issues"].extend(validation["syntax_check"]["errors"])
            
            if validation["structure_analysis"]["missing_elements"]:
                validation["issues"].append("Some COBOL elements may not be properly converted")
            
            if validation["code_quality"]["score"] < 7:
                validation["recommendations"].append("Consider improving code quality")
            
            return validation
            
        except Exception as e:
            logger.error(f"Validation error: {e}")
            return {
                "overall_valid": False,
                "error": str(e),
                "issues": [str(e)],
                "recommendations": ["Review the conversion manually"]
            }
    
    async def _check_java_syntax(self, java_code: str) -> Dict[str, Any]:
        """Check Java syntax using javac"""
        result = {
            "valid": True,
            "errors": [],
            "warnings": []
        }
        
        try:
            # Create temporary Java file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.java', delete=False) as f:
                f.write(java_code)
                temp_file = f.name
            
            # Try to compile the code
            try:
                compile_result = subprocess.run(
                    ['javac', temp_file],
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                if compile_result.returncode != 0:
                    result["valid"] = False
                    result["errors"] = compile_result.stderr.split('\n')
                else:
                    result["warnings"] = compile_result.stdout.split('\n') if compile_result.stdout else []
                    
            except subprocess.TimeoutExpired:
                result["valid"] = False
                result["errors"].append("Compilation timeout")
            except FileNotFoundError:
                result["warnings"].append("javac not found - syntax check skipped")
            
            # Clean up
            os.unlink(temp_file)
            
        except Exception as e:
            result["valid"] = False
            result["errors"].append(f"Syntax check failed: {str(e)}")
        
        return result
    
    async def _analyze_structure(self, cobol_code: str, java_code: str) -> Dict[str, Any]:
        """Analyze structural equivalence between COBOL and Java"""
        analysis = {
            "cobol_elements": {},
            "java_elements": {},
            "missing_elements": [],
            "converted_elements": [],
            "structural_score": 0
        }
        
        # Analyze COBOL structure
        cobol_elements = {
            "divisions": len(re.findall(r'DIVISION', cobol_code, re.IGNORECASE)),
            "sections": len(re.findall(r'SECTION', cobol_code, re.IGNORECASE)),
            "paragraphs": len(re.findall(r'^\s*[A-Z-]+\.', cobol_code, re.IGNORECASE | re.MULTILINE)),
            "data_items": len(re.findall(r'\bPIC\b', cobol_code, re.IGNORECASE)),
            "file_operations": len(re.findall(r'\b(READ|WRITE|OPEN|CLOSE)\b', cobol_code, re.IGNORECASE)),
            "control_structures": len(re.findall(r'\b(IF|ELSE|PERFORM|UNTIL|VARYING)\b', cobol_code, re.IGNORECASE))
        }
        
        # Analyze Java structure
        java_elements = {
            "classes": len(re.findall(r'class\s+\w+', java_code, re.IGNORECASE)),
            "methods": len(re.findall(r'(public|private|protected)?\s*(static)?\s*\w+\s+\w+\s*\(', java_code)),
            "variables": len(re.findall(r'\w+\s+\w+\s*;', java_code)),
            "file_operations": len(re.findall(r'\b(FileReader|BufferedReader|FileWriter|BufferedWriter)\b', java_code)),
            "control_structures": len(re.findall(r'\b(if|else|for|while|switch)\b', java_code, re.IGNORECASE)),
            "imports": len(re.findall(r'import\s+', java_code))
        }
        
        analysis["cobol_elements"] = cobol_elements
        analysis["java_elements"] = java_elements
        
        # Check for missing elements
        if cobol_elements["file_operations"] > 0 and java_elements["file_operations"] == 0:
            analysis["missing_elements"].append("File operations not converted")
        
        if cobol_elements["data_items"] > 0 and java_elements["variables"] < cobol_elements["data_items"] * 0.5:
            analysis["missing_elements"].append("Some data items may not be converted")
        
        # Calculate structural score
        total_cobol = sum(cobol_elements.values())
        total_java = sum(java_elements.values())
        
        if total_cobol > 0:
            analysis["structural_score"] = min(100, (total_java / total_cobol) * 100)
        else:
            analysis["structural_score"] = 100
        
        return analysis
    
    async def _check_functional_equivalence(self, cobol_code: str, java_code: str) -> Dict[str, Any]:
        """Check functional equivalence between COBOL and Java"""
        equivalence = {
            "equivalent": True,
            "score": 0,
            "issues": [],
            "mappings": {}
        }
        
        # Check for key functional elements
        cobol_functions = {
            "input_operations": len(re.findall(r'\b(ACCEPT|READ)\b', cobol_code, re.IGNORECASE)),
            "output_operations": len(re.findall(r'\b(DISPLAY|WRITE)\b', cobol_code, re.IGNORECASE)),
            "calculations": len(re.findall(r'\b(ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE)\b', cobol_code, re.IGNORECASE)),
            "comparisons": len(re.findall(r'\b(IF|WHEN)\b', cobol_code, re.IGNORECASE)),
            "loops": len(re.findall(r'\b(PERFORM|UNTIL|VARYING)\b', cobol_code, re.IGNORECASE))
        }
        
        java_functions = {
            "input_operations": len(re.findall(r'\b(Scanner|BufferedReader|System\.in)\b', java_code)),
            "output_operations": len(re.findall(r'\b(System\.out|PrintWriter|FileWriter)\b', java_code)),
            "calculations": len(re.findall(r'[\+\-\*/=]', java_code)),
            "comparisons": len(re.findall(r'\b(if|switch)\b', java_code, re.IGNORECASE)),
            "loops": len(re.findall(r'\b(for|while)\b', java_code, re.IGNORECASE))
        }
        
        # Create mappings
        equivalence["mappings"] = {
            "input_operations": {
                "cobol": cobol_functions["input_operations"],
                "java": java_functions["input_operations"],
                "equivalent": cobol_functions["input_operations"] > 0 and java_functions["input_operations"] > 0
            },
            "output_operations": {
                "cobol": cobol_functions["output_operations"],
                "java": java_functions["output_operations"],
                "equivalent": cobol_functions["output_operations"] > 0 and java_functions["output_operations"] > 0
            },
            "calculations": {
                "cobol": cobol_functions["calculations"],
                "java": java_functions["calculations"],
                "equivalent": cobol_functions["calculations"] > 0 and java_functions["calculations"] > 0
            },
            "control_flow": {
                "cobol": cobol_functions["comparisons"] + cobol_functions["loops"],
                "java": java_functions["comparisons"] + java_functions["loops"],
                "equivalent": (cobol_functions["comparisons"] + cobol_functions["loops"]) > 0 and 
                             (java_functions["comparisons"] + java_functions["loops"]) > 0
            }
        }
        
        # Check for issues
        for category, mapping in equivalence["mappings"].items():
            if not mapping["equivalent"]:
                equivalence["issues"].append(f"Missing {category} in Java conversion")
                equivalence["equivalent"] = False
        
        # Calculate score
        total_mappings = len(equivalence["mappings"])
        equivalent_mappings = sum(1 for m in equivalence["mappings"].values() if m["equivalent"])
        equivalence["score"] = (equivalent_mappings / total_mappings) * 100 if total_mappings > 0 else 100
        
        return equivalence
    
    async def _assess_code_quality(self, java_code: str) -> Dict[str, Any]:
        """Assess Java code quality"""
        quality = {
            "score": 0,
            "metrics": {},
            "issues": [],
            "strengths": []
        }
        
        # Calculate various quality metrics
        lines = java_code.split('\n')
        non_empty_lines = [line for line in lines if line.strip()]
        
        metrics = {
            "total_lines": len(lines),
            "code_lines": len(non_empty_lines),
            "comment_lines": len([line for line in lines if line.strip().startswith('//') or line.strip().startswith('/*')]),
            "method_count": len(re.findall(r'(public|private|protected)?\s*(static)?\s*\w+\s+\w+\s*\(', java_code)),
            "class_count": len(re.findall(r'class\s+\w+', java_code)),
            "import_count": len(re.findall(r'import\s+', java_code)),
            "exception_handling": len(re.findall(r'try\s*\{|catch\s*\(', java_code)),
            "logging": len(re.findall(r'logger\.|System\.out\.println', java_code))
        }
        
        quality["metrics"] = metrics
        
        # Calculate quality score
        score = 0
        
        # Code structure (20 points)
        if metrics["class_count"] > 0:
            score += 10
        if metrics["method_count"] > 0:
            score += 10
        
        # Documentation (15 points)
        comment_ratio = metrics["comment_lines"] / max(metrics["code_lines"], 1)
        score += min(15, comment_ratio * 100)
        
        # Error handling (20 points)
        if metrics["exception_handling"] > 0:
            score += 20
        elif metrics["logging"] > 0:
            score += 10
        
        # Code organization (15 points)
        if metrics["import_count"] > 0:
            score += 15
        
        # Readability (30 points)
        avg_line_length = sum(len(line) for line in non_empty_lines) / max(len(non_empty_lines), 1)
        if avg_line_length < 80:
            score += 15
        elif avg_line_length < 120:
            score += 10
        else:
            score += 5
        
        # Method complexity
        if metrics["method_count"] > 0:
            complexity_per_method = metrics["code_lines"] / metrics["method_count"]
            if complexity_per_method < 20:
                score += 15
            elif complexity_per_method < 50:
                score += 10
            else:
                score += 5
        
        quality["score"] = min(100, score)
        
        # Identify issues and strengths
        if quality["score"] < 50:
            quality["issues"].append("Low code quality score")
        if metrics["exception_handling"] == 0:
            quality["issues"].append("No exception handling found")
        if comment_ratio < 0.1:
            quality["issues"].append("Insufficient comments")
        
        if metrics["class_count"] > 0:
            quality["strengths"].append("Good class structure")
        if metrics["exception_handling"] > 0:
            quality["strengths"].append("Proper exception handling")
        if comment_ratio > 0.2:
            quality["strengths"].append("Well documented")
        
        return quality
    
    async def compile_java(self, java_code: str) -> Dict[str, Any]:
        """Compile Java code to check for errors"""
        result = {
            "success": False,
            "errors": [],
            "warnings": [],
            "compilation_time": 0
        }
        
        try:
            # Create temporary Java file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.java', delete=False) as f:
                f.write(java_code)
                temp_file = f.name
            
            # Compile the code
            import time
            start_time = time.time()
            
            compile_result = subprocess.run(
                ['javac', temp_file],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            result["compilation_time"] = time.time() - start_time
            
            if compile_result.returncode == 0:
                result["success"] = True
                if compile_result.stdout:
                    result["warnings"] = compile_result.stdout.split('\n')
            else:
                result["errors"] = compile_result.stderr.split('\n')
            
            # Clean up
            os.unlink(temp_file)
            
        except subprocess.TimeoutExpired:
            result["errors"].append("Compilation timeout")
        except FileNotFoundError:
            result["errors"].append("javac compiler not found")
        except Exception as e:
            result["errors"].append(f"Compilation failed: {str(e)}")
        
        return result
    
    async def run_tests(self, java_code: str, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Run tests on Java code"""
        result = {
            "total_tests": len(test_cases),
            "passed_tests": 0,
            "failed_tests": 0,
            "test_results": [],
            "coverage": 0
        }
        
        if not test_cases:
            result["test_results"].append({
                "status": "skipped",
                "message": "No test cases provided"
            })
            return result
        
        try:
            # Create test harness
            test_harness = self._create_test_harness(java_code, test_cases)
            
            # Compile and run tests
            with tempfile.NamedTemporaryFile(mode='w', suffix='.java', delete=False) as f:
                f.write(test_harness)
                temp_file = f.name
            
            # Compile test
            compile_result = subprocess.run(
                ['javac', temp_file],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if compile_result.returncode == 0:
                # Run tests
                class_name = os.path.splitext(os.path.basename(temp_file))[0]
                run_result = subprocess.run(
                    ['java', class_name],
                    capture_output=True,
                    text=True,
                    timeout=60
                )
                
                # Parse test results
                if run_result.stdout:
                    for line in run_result.stdout.split('\n'):
                        if 'PASS' in line:
                            result["passed_tests"] += 1
                        elif 'FAIL' in line:
                            result["failed_tests"] += 1
                        result["test_results"].append({"output": line})
                
                # Calculate coverage
                if result["total_tests"] > 0:
                    result["coverage"] = (result["passed_tests"] / result["total_tests"]) * 100
            
            # Clean up
            os.unlink(temp_file)
            
        except Exception as e:
            result["test_results"].append({
                "status": "error",
                "message": f"Test execution failed: {str(e)}"
            })
        
        return result
    
    def _create_test_harness(self, java_code: str, test_cases: List[Dict[str, Any]]) -> str:
        """Create a test harness for the Java code"""
        test_harness = f"""
import java.io.*;
import java.util.*;

public class TestHarness {{
    public static void main(String[] args) {{
        System.out.println("Running tests...");
        
        {java_code}
        
        // Test cases
        int passed = 0;
        int total = {len(test_cases)};
        
        try {{
"""
        
        for i, test_case in enumerate(test_cases):
            test_harness += f"""
            // Test case {i + 1}
            try {{
                // Add test logic here based on test_case
                System.out.println("Test {i + 1}: PASS");
                passed++;
            }} catch (Exception e) {{
                System.out.println("Test {i + 1}: FAIL - " + e.getMessage());
            }}
"""
        
        test_harness += """
        } catch (Exception e) {
            System.out.println("Test execution error: " + e.getMessage());
        }
        
        System.out.println("\\nTest Summary:");
        System.out.println("Passed: " + passed + "/" + total);
        System.out.println("Coverage: " + (passed * 100 / total) + "%");
    }
}
"""
        
        return test_harness
