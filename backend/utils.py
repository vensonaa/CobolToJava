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
        logging.FileHandler('cobol_converter.log'),
        logging.StreamHandler()
    ]
)

logger = logging.getLogger(__name__)

def load_environment_variables():
    """Load environment variables from .env file"""
    from dotenv import load_dotenv
    load_dotenv()
    
    return {
        "OPENAI_API_KEY": os.getenv("OPENAI_API_KEY"),
        "GROQ_API_KEY": os.getenv("GROQ_API_KEY"),
        "ANTHROPIC_API_KEY": os.getenv("ANTHROPIC_API_KEY"),
        "MCP_SERVER_URL": os.getenv("MCP_SERVER_URL", "http://localhost:8001"),
        "DATABASE_URL": os.getenv("DATABASE_URL", "cobol_converter.db"),
        "LOG_LEVEL": os.getenv("LOG_LEVEL", "INFO")
    }

def validate_cobol_code(cobol_code: str) -> Dict[str, Any]:
    """Validate COBOL code structure"""
    validation_result = {
        "is_valid": True,
        "errors": [],
        "warnings": [],
        "structure": {}
    }
    
    if not cobol_code.strip():
        validation_result["is_valid"] = False
        validation_result["errors"].append("Empty COBOL code")
        return validation_result
    
    # Basic COBOL structure validation
    lines = cobol_code.split('\n')
    
    # Check for division headers
    divisions = []
    for line in lines:
        if re.search(r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION', line, re.IGNORECASE):
            divisions.append(line.strip())
    
    if not divisions:
        validation_result["warnings"].append("No standard COBOL divisions found")
    
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
        validation_result["warnings"].append("Few COBOL keywords detected - may not be valid COBOL")
    
    validation_result["structure"] = {
        "divisions": divisions,
        "keywords_found": found_keywords,
        "line_count": len(lines)
    }
    
    return validation_result

def analyze_cobol_complexity(cobol_code: str) -> Dict[str, Any]:
    """Analyze COBOL code complexity"""
    analysis = {
        "complexity_score": 0,
        "metrics": {},
        "challenges": []
    }
    
    lines = cobol_code.split('\n')
    non_empty_lines = [line for line in lines if line.strip()]
    
    # Basic metrics
    analysis["metrics"] = {
        "total_lines": len(lines),
        "code_lines": len(non_empty_lines),
        "comment_lines": len([line for line in lines if line.strip().startswith('*')]),
        "blank_lines": len(lines) - len(non_empty_lines)
    }
    
    # Complexity indicators
    complexity_indicators = {
        "file_operations": len(re.findall(r'\b(READ|WRITE|OPEN|CLOSE)\b', cobol_code, re.IGNORECASE)),
        "control_structures": len(re.findall(r'\b(IF|ELSE|PERFORM|UNTIL|VARYING)\b', cobol_code, re.IGNORECASE)),
        "data_structures": len(re.findall(r'\b(PIC|OCCURS|REDEFINES)\b', cobol_code, re.IGNORECASE)),
        "subroutines": len(re.findall(r'\b(CALL|PERFORM)\b', cobol_code, re.IGNORECASE))
    }
    
    analysis["metrics"].update(complexity_indicators)
    
    # Calculate complexity score
    score = 0
    score += complexity_indicators["file_operations"] * 2
    score += complexity_indicators["control_structures"] * 1
    score += complexity_indicators["data_structures"] * 3
    score += complexity_indicators["subroutines"] * 2
    score += len(non_empty_lines) * 0.1
    
    analysis["complexity_score"] = min(score, 100)  # Cap at 100
    
    # Identify potential challenges
    if complexity_indicators["file_operations"] > 5:
        analysis["challenges"].append("Complex file handling")
    
    if complexity_indicators["data_structures"] > 10:
        analysis["challenges"].append("Complex data structures")
    
    if complexity_indicators["control_structures"] > 20:
        analysis["challenges"].append("Complex control flow")
    
    if len(non_empty_lines) > 500:
        analysis["challenges"].append("Large codebase")
    
    return analysis

def generate_conversion_guidelines(cobol_analysis: Dict[str, Any]) -> Dict[str, Any]:
    """Generate conversion guidelines based on COBOL analysis"""
    guidelines = {
        "recommended_approach": "standard",
        "estimated_effort": "medium",
        "key_considerations": [],
        "testing_strategy": "comprehensive",
        "risk_level": "medium"
    }
    
    complexity_score = cobol_analysis.get("complexity_score", 0)
    challenges = cobol_analysis.get("challenges", [])
    
    if complexity_score > 70:
        guidelines["recommended_approach"] = "incremental"
        guidelines["estimated_effort"] = "high"
        guidelines["risk_level"] = "high"
        guidelines["testing_strategy"] = "extensive"
    elif complexity_score > 40:
        guidelines["recommended_approach"] = "modular"
        guidelines["estimated_effort"] = "medium"
        guidelines["risk_level"] = "medium"
    else:
        guidelines["recommended_approach"] = "direct"
        guidelines["estimated_effort"] = "low"
        guidelines["risk_level"] = "low"
        guidelines["testing_strategy"] = "basic"
    
    # Add specific considerations based on challenges
    for challenge in challenges:
        if "file handling" in challenge.lower():
            guidelines["key_considerations"].append("Use Java NIO for file operations")
        elif "data structures" in challenge.lower():
            guidelines["key_considerations"].append("Map COBOL data types to Java equivalents carefully")
        elif "control flow" in challenge.lower():
            guidelines["key_considerations"].append("Simplify nested control structures")
        elif "large codebase" in challenge.lower():
            guidelines["key_considerations"].append("Consider breaking into smaller modules")
    
    return guidelines

def format_java_code(java_code: str) -> str:
    """Format Java code for better readability"""
    # Basic Java formatting
    lines = java_code.split('\n')
    formatted_lines = []
    indent_level = 0
    
    for line in lines:
        stripped = line.strip()
        if not stripped:
            formatted_lines.append('')
            continue
        
        # Decrease indent for closing braces
        if stripped.startswith('}'):
            indent_level = max(0, indent_level - 1)
        
        # Add indentation
        formatted_line = '    ' * indent_level + stripped
        formatted_lines.append(formatted_line)
        
        # Increase indent for opening braces
        if stripped.endswith('{'):
            indent_level += 1
    
    return '\n'.join(formatted_lines)

def extract_java_package_and_imports(java_code: str) -> Dict[str, Any]:
    """Extract package declaration and imports from Java code"""
    result = {
        "package": None,
        "imports": [],
        "main_class": None
    }
    
    lines = java_code.split('\n')
    
    for line in lines:
        stripped = line.strip()
        if stripped.startswith('package '):
            result["package"] = stripped.replace('package ', '').replace(';', '')
        elif stripped.startswith('import '):
            import_statement = stripped.replace('import ', '').replace(';', '')
            result["imports"].append(import_statement)
        elif 'public class ' in stripped and 'main' in java_code:
            # Extract class name
            class_match = re.search(r'public class (\w+)', stripped)
            if class_match:
                result["main_class"] = class_match.group(1)
    
    return result

def create_java_project_structure(java_code: str, project_name: str = "ConvertedProject") -> Dict[str, str]:
    """Create a complete Java project structure"""
    structure = {}
    
    # Extract package and imports
    code_info = extract_java_package_and_imports(java_code)
    
    # Create main Java file
    if code_info["package"]:
        package_path = code_info["package"].replace('.', '/')
        main_file_path = f"src/main/java/{package_path}/Main.java"
    else:
        main_file_path = "src/main/java/Main.java"
    
    structure[main_file_path] = java_code
    
    # Create pom.xml for Maven
    pom_xml = f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>com.converted</groupId>
    <artifactId>{project_name.lower()}</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    
    <name>{project_name}</name>
    <description>Java code converted from COBOL</description>
    
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    
    <dependencies>
        <dependency>
            <groupId>junit</groupId>
            <artifactId>junit</artifactId>
            <version>4.13.2</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
    
    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.8.1</version>
                <configuration>
                    <source>11</source>
                    <target>11</target>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>"""
    
    structure["pom.xml"] = pom_xml
    
    # Create README
    readme = f"""# {project_name}

This project contains Java code converted from COBOL using an AI-powered conversion system.

## Building the Project

```bash
mvn clean compile
```

## Running the Application

```bash
mvn exec:java -Dexec.mainClass="Main"
```

## Testing

```bash
mvn test
```

## Conversion Details

- **Original Language**: COBOL
- **Target Language**: Java
- **Conversion Date**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
- **Conversion Method**: AI-powered with human review

## Notes

This code was automatically converted and may require manual review and testing.
"""
    
    structure["README.md"] = readme
    
    return structure

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

def create_backup_filename(original_filename: str) -> str:
    """Create a backup filename with timestamp"""
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    name, ext = os.path.splitext(original_filename)
    return f"{name}_backup_{timestamp}{ext}"
