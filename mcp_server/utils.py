import logging
import os
import json
from datetime import datetime
from typing import Dict, Any, Optional
import re

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('mcp_server.log'),
        logging.StreamHandler()
    ]
)

logger = logging.getLogger(__name__)

def load_environment_variables():
    """Load environment variables from .env file"""
    from dotenv import load_dotenv
    load_dotenv()
    
    return {
        "MCP_SERVER_PORT": int(os.getenv("MCP_SERVER_PORT", "8001")),
        "LOG_LEVEL": os.getenv("LOG_LEVEL", "INFO"),
        "ENABLE_STREAMING": os.getenv("ENABLE_STREAMING", "true").lower() == "true"
    }

def validate_cobol_syntax(cobol_code: str) -> Dict[str, Any]:
    """Basic COBOL syntax validation"""
    validation = {
        "is_valid": True,
        "errors": [],
        "warnings": [],
        "structure": {}
    }
    
    if not cobol_code.strip():
        validation["is_valid"] = False
        validation["errors"].append("Empty COBOL code")
        return validation
    
    lines = cobol_code.split('\n')
    
    # Check for basic COBOL structure
    has_identification = False
    has_procedure = False
    
    for line in lines:
        line_upper = line.upper().strip()
        if 'IDENTIFICATION DIVISION' in line_upper:
            has_identification = True
        elif 'PROCEDURE DIVISION' in line_upper:
            has_procedure = True
    
    if not has_identification:
        validation["warnings"].append("Missing IDENTIFICATION DIVISION")
    
    if not has_procedure:
        validation["warnings"].append("Missing PROCEDURE DIVISION")
    
    # Check for common COBOL keywords
    cobol_keywords = [
        'PROGRAM-ID', 'WORKING-STORAGE', 'PROCEDURE', 'END PROGRAM',
        'MOVE', 'DISPLAY', 'ACCEPT', 'IF', 'ELSE', 'END-IF',
        'PERFORM', 'UNTIL', 'VARYING', 'READ', 'WRITE', 'OPEN', 'CLOSE'
    ]
    
    found_keywords = []
    for keyword in cobol_keywords:
        if re.search(rf'\b{keyword}\b', cobol_code, re.IGNORECASE):
            found_keywords.append(keyword)
    
    if len(found_keywords) < 3:
        validation["warnings"].append("Few COBOL keywords detected")
    
    validation["structure"] = {
        "has_identification": has_identification,
        "has_procedure": has_procedure,
        "keywords_found": found_keywords,
        "line_count": len(lines)
    }
    
    return validation

def analyze_java_syntax(java_code: str) -> Dict[str, Any]:
    """Basic Java syntax analysis"""
    analysis = {
        "is_valid": True,
        "errors": [],
        "warnings": [],
        "structure": {}
    }
    
    if not java_code.strip():
        analysis["is_valid"] = False
        analysis["errors"].append("Empty Java code")
        return analysis
    
    lines = java_code.split('\n')
    
    # Check for basic Java structure
    has_class = False
    has_main = False
    brace_count = 0
    
    for line in lines:
        line_stripped = line.strip()
        
        if 'class' in line_stripped and 'public' in line_stripped:
            has_class = True
        
        if 'public static void main' in line_stripped:
            has_main = True
        
        # Count braces for basic syntax check
        brace_count += line_stripped.count('{') - line_stripped.count('}')
    
    if not has_class:
        analysis["warnings"].append("No public class found")
    
    if not has_main:
        analysis["warnings"].append("No main method found")
    
    if brace_count != 0:
        analysis["warnings"].append("Unmatched braces detected")
    
    # Check for common Java keywords
    java_keywords = [
        'public', 'private', 'protected', 'class', 'interface',
        'static', 'final', 'void', 'int', 'String', 'boolean',
        'if', 'else', 'for', 'while', 'try', 'catch', 'import'
    ]
    
    found_keywords = []
    for keyword in java_keywords:
        if re.search(rf'\b{keyword}\b', java_code, re.IGNORECASE):
            found_keywords.append(keyword)
    
    analysis["structure"] = {
        "has_class": has_class,
        "has_main": has_main,
        "brace_balance": brace_count == 0,
        "keywords_found": found_keywords,
        "line_count": len(lines)
    }
    
    return analysis

def extract_cobol_elements(cobol_code: str) -> Dict[str, Any]:
    """Extract key elements from COBOL code"""
    elements = {
        "divisions": [],
        "sections": [],
        "paragraphs": [],
        "data_items": [],
        "file_operations": [],
        "control_structures": []
    }
    
    lines = cobol_code.split('\n')
    
    for i, line in enumerate(lines):
        line_upper = line.upper().strip()
        
        # Identify divisions
        if 'DIVISION' in line_upper:
            elements["divisions"].append({
                "name": line_upper,
                "line": i + 1
            })
        
        # Identify sections
        elif 'SECTION' in line_upper:
            elements["sections"].append({
                "name": line_upper,
                "line": i + 1
            })
        
        # Identify paragraphs
        elif re.match(r'^\s*[A-Z-]+\.', line_upper):
            paragraph_name = line_upper.split('.')[0].strip()
            elements["paragraphs"].append({
                "name": paragraph_name,
                "line": i + 1
            })
        
        # Identify data items
        elif 'PIC' in line_upper:
            pic_match = re.search(r'(\d{2})\s+([A-Z-]+)\s+PIC', line_upper)
            if pic_match:
                elements["data_items"].append({
                    "level": pic_match.group(1),
                    "name": pic_match.group(2),
                    "line": i + 1
                })
        
        # Identify file operations
        file_ops = ['READ', 'WRITE', 'OPEN', 'CLOSE']
        for op in file_ops:
            if op in line_upper:
                elements["file_operations"].append({
                    "operation": op,
                    "line": i + 1
                })
        
        # Identify control structures
        control_ops = ['IF', 'ELSE', 'PERFORM', 'UNTIL', 'VARYING']
        for op in control_ops:
            if op in line_upper:
                elements["control_structures"].append({
                    "structure": op,
                    "line": i + 1
                })
    
    return elements

def extract_java_elements(java_code: str) -> Dict[str, Any]:
    """Extract key elements from Java code"""
    elements = {
        "classes": [],
        "methods": [],
        "variables": [],
        "imports": [],
        "file_operations": [],
        "control_structures": []
    }
    
    lines = java_code.split('\n')
    
    for i, line in enumerate(lines):
        line_stripped = line.strip()
        
        # Identify classes
        class_match = re.search(r'class\s+(\w+)', line_stripped)
        if class_match:
            elements["classes"].append({
                "name": class_match.group(1),
                "line": i + 1
            })
        
        # Identify methods
        method_match = re.search(r'(public|private|protected)?\s*(static)?\s*(\w+)\s+(\w+)\s*\(', line_stripped)
        if method_match:
            elements["methods"].append({
                "modifier": method_match.group(1) or "default",
                "static": bool(method_match.group(2)),
                "return_type": method_match.group(3),
                "name": method_match.group(4),
                "line": i + 1
            })
        
        # Identify variables
        var_match = re.search(r'(\w+)\s+(\w+)\s*;', line_stripped)
        if var_match:
            elements["variables"].append({
                "type": var_match.group(1),
                "name": var_match.group(2),
                "line": i + 1
            })
        
        # Identify imports
        if line_stripped.startswith('import '):
            elements["imports"].append({
                "package": line_stripped.replace('import ', '').replace(';', ''),
                "line": i + 1
            })
        
        # Identify file operations
        file_ops = ['FileReader', 'BufferedReader', 'FileWriter', 'BufferedWriter', 'Scanner']
        for op in file_ops:
            if op in line_stripped:
                elements["file_operations"].append({
                    "operation": op,
                    "line": i + 1
                })
        
        # Identify control structures
        control_ops = ['if', 'else', 'for', 'while', 'switch']
        for op in control_ops:
            if op in line_stripped:
                elements["control_structures"].append({
                    "structure": op,
                    "line": i + 1
                })
    
    return elements

def calculate_complexity_score(cobol_code: str) -> Dict[str, Any]:
    """Calculate complexity score for COBOL code"""
    score = 0
    metrics = {}
    
    lines = cobol_code.split('\n')
    non_empty_lines = [line for line in lines if line.strip()]
    
    # Basic metrics
    metrics["total_lines"] = len(lines)
    metrics["code_lines"] = len(non_empty_lines)
    metrics["comment_lines"] = len([line for line in lines if line.strip().startswith('*')])
    metrics["blank_lines"] = len(lines) - len(non_empty_lines)
    
    # Complexity indicators
    complexity_indicators = {
        "file_operations": len(re.findall(r'\b(READ|WRITE|OPEN|CLOSE)\b', cobol_code, re.IGNORECASE)),
        "control_structures": len(re.findall(r'\b(IF|ELSE|PERFORM|UNTIL|VARYING)\b', cobol_code, re.IGNORECASE)),
        "data_structures": len(re.findall(r'\b(PIC|OCCURS|REDEFINES)\b', cobol_code, re.IGNORECASE)),
        "subroutines": len(re.findall(r'\b(CALL|PERFORM)\b', cobol_code, re.IGNORECASE))
    }
    
    metrics.update(complexity_indicators)
    
    # Calculate score
    score += complexity_indicators["file_operations"] * 2
    score += complexity_indicators["control_structures"] * 1
    score += complexity_indicators["data_structures"] * 3
    score += complexity_indicators["subroutines"] * 2
    score += len(non_empty_lines) * 0.1
    
    return {
        "complexity_score": min(score, 100),
        "metrics": metrics,
        "level": "low" if score < 30 else "medium" if score < 70 else "high"
    }

def generate_conversion_mapping() -> Dict[str, str]:
    """Generate COBOL to Java conversion mappings"""
    return {
        "data_types": {
            "PIC X(n)": "String",
            "PIC 9(n)": "int/long",
            "PIC 9(n)V9(m)": "BigDecimal",
            "PIC S9(n)": "int/long",
            "PIC S9(n)V9(m)": "BigDecimal"
        },
        "control_structures": {
            "IF ... ELSE ... END-IF": "if (...) { ... } else { ... }",
            "PERFORM UNTIL": "while (!condition) { ... }",
            "PERFORM VARYING": "for (int i = start; i <= end; i++) { ... }",
            "EVALUATE": "switch (...) { case ...: ... break; }"
        },
        "file_operations": {
            "OPEN INPUT": "FileReader/BufferedReader",
            "OPEN OUTPUT": "FileWriter/BufferedWriter",
            "READ": "readLine()",
            "WRITE": "write()/println()",
            "CLOSE": "close()"
        },
        "arithmetic": {
            "ADD": "+",
            "SUBTRACT": "-",
            "MULTIPLY": "*",
            "DIVIDE": "/",
            "COMPUTE": "="
        },
        "input_output": {
            "ACCEPT": "Scanner.nextLine()",
            "DISPLAY": "System.out.println()"
        }
    }

def create_sample_cobol_code() -> str:
    """Create a sample COBOL program for testing"""
    return """       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WS-NAME     PIC X(20).
           01  WS-COUNTER  PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter your name: ".
           ACCEPT WS-NAME.
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 3
               DISPLAY "Hello, " WS-NAME
           END-PERFORM.
           
           IF WS-COUNTER > 2
               DISPLAY "Counter is greater than 2"
           ELSE
               DISPLAY "Counter is 2 or less"
           END-IF.
           
           STOP RUN."""

def create_sample_java_code() -> str:
    """Create a sample Java program for testing"""
    return """import java.util.Scanner;

public class HelloWorld {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String name;
        int counter;
        
        System.out.print("Enter your name: ");
        name = scanner.nextLine();
        
        for (counter = 1; counter <= 3; counter++) {
            System.out.println("Hello, " + name);
        }
        
        if (counter > 2) {
            System.out.println("Counter is greater than 2");
        } else {
            System.out.println("Counter is 2 or less");
        }
        
        scanner.close();
    }
}"""

def format_timestamp() -> str:
    """Format current timestamp"""
    return datetime.now().strftime('%Y-%m-%d %H:%M:%S')

def sanitize_filename(filename: str) -> str:
    """Sanitize filename for safe file operations"""
    # Remove or replace invalid characters
    sanitized = re.sub(r'[<>:"/\\|?*]', '_', filename)
    # Remove leading/trailing spaces and dots
    sanitized = sanitized.strip('. ')
    # Limit length
    if len(sanitized) > 255:
        sanitized = sanitized[:255]
    return sanitized
