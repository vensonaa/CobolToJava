import re
import json
import asyncio
from typing import Dict, Any, List, Optional
from utils import logger

class COBOLAnalyzer:
    def __init__(self):
        self.ready = True
        self.analysis_cache = {}
        
    def is_ready(self) -> bool:
        """Check if analyzer is ready"""
        return self.ready
    
    async def analyze(self, code: str, analysis_type: str = "general") -> Dict[str, Any]:
        """Comprehensive COBOL code analysis"""
        try:
            analysis = {
                "validation": await self.validate_code(code),
                "complexity": await self.analyze_complexity(code),
                "structure": await self.analyze_structure(code),
                "conversion_plan": await self.create_conversion_plan(code),
                "suggestions": await self.get_conversion_suggestions(code),
                "best_practices": await self.get_best_practices({"code": code, "type": analysis_type})
            }
            
            # Cache the analysis
            cache_key = f"{hash(code)}_{analysis_type}"
            self.analysis_cache[cache_key] = analysis
            
            return analysis
            
        except Exception as e:
            logger.error(f"COBOL analysis error: {e}")
            return {"error": str(e)}
    
    async def validate_code(self, code: str) -> Dict[str, Any]:
        """Validate COBOL code structure"""
        validation = {
            "is_valid": True,
            "errors": [],
            "warnings": [],
            "structure": {}
        }
        
        if not code.strip():
            validation["is_valid"] = False
            validation["errors"].append("Empty COBOL code")
            return validation
        
        lines = code.split('\n')
        
        # Check for division headers
        divisions = []
        for line in lines:
            if re.search(r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION', line, re.IGNORECASE):
                divisions.append(line.strip())
        
        if not divisions:
            validation["warnings"].append("No standard COBOL divisions found")
        
        # Check for common COBOL keywords
        cobol_keywords = [
            'PROGRAM-ID', 'WORKING-STORAGE', 'PROCEDURE', 'END PROGRAM',
            'MOVE', 'DISPLAY', 'ACCEPT', 'IF', 'ELSE', 'END-IF',
            'PERFORM', 'UNTIL', 'VARYING', 'READ', 'WRITE', 'OPEN', 'CLOSE'
        ]
        
        found_keywords = []
        for keyword in cobol_keywords:
            if re.search(rf'\b{keyword}\b', code, re.IGNORECASE):
                found_keywords.append(keyword)
        
        if len(found_keywords) < 3:
            validation["warnings"].append("Few COBOL keywords detected - may not be valid COBOL")
        
        validation["structure"] = {
            "divisions": divisions,
            "keywords_found": found_keywords,
            "line_count": len(lines)
        }
        
        return validation
    
    async def analyze_complexity(self, code: str) -> Dict[str, Any]:
        """Analyze COBOL code complexity"""
        analysis = {
            "complexity_score": 0,
            "metrics": {},
            "challenges": []
        }
        
        lines = code.split('\n')
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
            "file_operations": len(re.findall(r'\b(READ|WRITE|OPEN|CLOSE)\b', code, re.IGNORECASE)),
            "control_structures": len(re.findall(r'\b(IF|ELSE|PERFORM|UNTIL|VARYING)\b', code, re.IGNORECASE)),
            "data_structures": len(re.findall(r'\b(PIC|OCCURS|REDEFINES)\b', code, re.IGNORECASE)),
            "subroutines": len(re.findall(r'\b(CALL|PERFORM)\b', code, re.IGNORECASE))
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
    
    async def analyze_structure(self, code: str) -> Dict[str, Any]:
        """Analyze COBOL code structure"""
        structure = {
            "divisions": {},
            "sections": {},
            "paragraphs": {},
            "data_items": {},
            "file_handling": {},
            "control_flow": {}
        }
        
        lines = code.split('\n')
        current_division = None
        current_section = None
        
        for i, line in enumerate(lines):
            line = line.strip()
            
            # Identify divisions
            if re.search(r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION', line, re.IGNORECASE):
                current_division = line
                structure["divisions"][current_division] = {"start_line": i + 1}
            
            # Identify sections
            elif re.search(r'^\s*[A-Z-]+\s+SECTION', line, re.IGNORECASE):
                current_section = line
                if current_division:
                    if "sections" not in structure["divisions"][current_division]:
                        structure["divisions"][current_division]["sections"] = {}
                    structure["divisions"][current_division]["sections"][current_section] = {"start_line": i + 1}
            
            # Identify paragraphs
            elif re.search(r'^\s*[A-Z-]+\.', line, re.IGNORECASE):
                paragraph = line.split('.')[0].strip()
                if current_section:
                    if "paragraphs" not in structure["divisions"][current_division]["sections"][current_section]:
                        structure["divisions"][current_division]["sections"][current_section]["paragraphs"] = {}
                    structure["divisions"][current_division]["sections"][current_section]["paragraphs"][paragraph] = {"start_line": i + 1}
            
            # Identify data items
            elif re.search(r'\bPIC\b', line, re.IGNORECASE):
                data_item_match = re.search(r'(\d{2})\s+([A-Z-]+)\s+PIC', line, re.IGNORECASE)
                if data_item_match:
                    level = data_item_match.group(1)
                    name = data_item_match.group(2)
                    structure["data_items"][name] = {"level": level, "line": i + 1}
        
        # Analyze file handling
        file_operations = re.findall(r'\b(OPEN|CLOSE|READ|WRITE)\b', code, re.IGNORECASE)
        structure["file_handling"]["operations"] = file_operations
        structure["file_handling"]["count"] = len(file_operations)
        
        # Analyze control flow
        control_structures = re.findall(r'\b(IF|ELSE|PERFORM|UNTIL|VARYING|CALL)\b', code, re.IGNORECASE)
        structure["control_flow"]["structures"] = control_structures
        structure["control_flow"]["count"] = len(control_structures)
        
        return structure
    
    async def create_conversion_plan(self, code: str) -> Dict[str, Any]:
        """Create a conversion plan for COBOL to Java"""
        plan = {
            "approach": "modular",
            "estimated_effort": "medium",
            "key_considerations": [],
            "testing_strategy": "comprehensive",
            "risk_level": "medium",
            "conversion_steps": []
        }
        
        # Analyze complexity to determine approach
        complexity = await self.analyze_complexity(code)
        complexity_score = complexity.get("complexity_score", 0)
        
        if complexity_score > 70:
            plan["approach"] = "incremental"
            plan["estimated_effort"] = "high"
            plan["risk_level"] = "high"
            plan["testing_strategy"] = "extensive"
        elif complexity_score > 40:
            plan["approach"] = "modular"
            plan["estimated_effort"] = "medium"
            plan["risk_level"] = "medium"
        else:
            plan["approach"] = "direct"
            plan["estimated_effort"] = "low"
            plan["risk_level"] = "low"
            plan["testing_strategy"] = "basic"
        
        # Add specific considerations based on code analysis
        challenges = complexity.get("challenges", [])
        for challenge in challenges:
            if "file handling" in challenge.lower():
                plan["key_considerations"].append("Use Java NIO for file operations")
            elif "data structures" in challenge.lower():
                plan["key_considerations"].append("Map COBOL data types to Java equivalents carefully")
            elif "control flow" in challenge.lower():
                plan["key_considerations"].append("Simplify nested control structures")
            elif "large codebase" in challenge.lower():
                plan["key_considerations"].append("Consider breaking into smaller modules")
        
        # Define conversion steps
        plan["conversion_steps"] = [
            "Analyze COBOL structure and identify components",
            "Map COBOL data types to Java equivalents",
            "Convert file handling operations",
            "Transform control flow structures",
            "Implement error handling",
            "Add Java-specific optimizations",
            "Generate comprehensive tests",
            "Perform code review and validation"
        ]
        
        return plan
    
    async def get_conversion_suggestions(self, code: str) -> List[Dict[str, Any]]:
        """Get specific conversion suggestions"""
        suggestions = []
        
        # Analyze code patterns and provide suggestions
        if re.search(r'\bPIC\s+X\b', code, re.IGNORECASE):
            suggestions.append({
                "type": "data_type",
                "message": "Convert PIC X to String",
                "priority": "high",
                "example": "PIC X(10) -> String"
            })
        
        if re.search(r'\bPIC\s+9\b', code, re.IGNORECASE):
            suggestions.append({
                "type": "data_type",
                "message": "Convert PIC 9 to appropriate numeric types",
                "priority": "high",
                "example": "PIC 9(5) -> int, PIC 9(15) -> long"
            })
        
        if re.search(r'\bPERFORM\s+UNTIL\b', code, re.IGNORECASE):
            suggestions.append({
                "type": "control_flow",
                "message": "Convert PERFORM UNTIL to while loop",
                "priority": "medium",
                "example": "PERFORM UNTIL condition -> while (!condition)"
            })
        
        if re.search(r'\bREAD\b.*\bAT\s+END\b', code, re.IGNORECASE):
            suggestions.append({
                "type": "file_handling",
                "message": "Use try-with-resources for file operations",
                "priority": "high",
                "example": "READ file AT END -> try (BufferedReader reader = ...)"
            })
        
        if re.search(r'\bMOVE\b', code, re.IGNORECASE):
            suggestions.append({
                "type": "assignment",
                "message": "Convert MOVE to assignment operator",
                "priority": "low",
                "example": "MOVE value TO variable -> variable = value"
            })
        
        return suggestions
    
    async def get_best_practices(self, context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Get best practices for COBOL to Java conversion"""
        best_practices = [
            {
                "category": "data_types",
                "practice": "Use appropriate Java data types",
                "description": "Map COBOL data types to their closest Java equivalents",
                "examples": [
                    "PIC X(n) -> String",
                    "PIC 9(n) -> int/long",
                    "PIC 9(n)V9(m) -> BigDecimal"
                ]
            },
            {
                "category": "file_handling",
                "practice": "Use modern Java file APIs",
                "description": "Replace COBOL file operations with Java NIO",
                "examples": [
                    "Use BufferedReader for sequential reads",
                    "Use try-with-resources for automatic cleanup",
                    "Use Path API for file operations"
                ]
            },
            {
                "category": "error_handling",
                "practice": "Implement proper exception handling",
                "description": "Convert COBOL error handling to Java exceptions",
                "examples": [
                    "Use try-catch blocks",
                    "Create custom exceptions for business logic",
                    "Log errors appropriately"
                ]
            },
            {
                "category": "code_structure",
                "practice": "Follow Java naming conventions",
                "description": "Convert COBOL naming to Java conventions",
                "examples": [
                    "Use camelCase for variables and methods",
                    "Use PascalCase for classes",
                    "Use UPPER_CASE for constants"
                ]
            },
            {
                "category": "testing",
                "practice": "Write comprehensive unit tests",
                "description": "Ensure converted code works correctly",
                "examples": [
                    "Test all code paths",
                    "Use JUnit for testing framework",
                    "Include edge case testing"
                ]
            }
        ]
        
        # Add context-specific practices
        if context.get("type") == "complex":
            best_practices.append({
                "category": "complexity",
                "practice": "Break down complex logic",
                "description": "Split complex COBOL procedures into smaller Java methods",
                "examples": [
                    "Extract helper methods",
                    "Use design patterns where appropriate",
                    "Consider refactoring opportunities"
                ]
            })
        
        return best_practices
    
    async def estimate_complexity(self, code: str) -> Dict[str, Any]:
        """Estimate conversion complexity"""
        complexity_analysis = await self.analyze_complexity(code)
        
        estimation = {
            "overall_complexity": "medium",
            "estimated_hours": 8,
            "risk_factors": [],
            "recommendations": []
        }
        
        complexity_score = complexity_analysis.get("complexity_score", 0)
        
        if complexity_score > 70:
            estimation["overall_complexity"] = "high"
            estimation["estimated_hours"] = 24
            estimation["risk_factors"].append("High complexity score")
            estimation["recommendations"].append("Consider incremental conversion")
        elif complexity_score > 40:
            estimation["overall_complexity"] = "medium"
            estimation["estimated_hours"] = 12
            estimation["risk_factors"].append("Medium complexity")
            estimation["recommendations"].append("Plan for thorough testing")
        else:
            estimation["overall_complexity"] = "low"
            estimation["estimated_hours"] = 4
            estimation["recommendations"].append("Standard conversion approach")
        
        # Add specific risk factors based on analysis
        challenges = complexity_analysis.get("challenges", [])
        for challenge in challenges:
            estimation["risk_factors"].append(challenge)
        
        return estimation
    
    async def generate_documentation(self, cobol_code: str, java_code: str) -> str:
        """Generate documentation for the conversion"""
        documentation = f"""# COBOL to Java Conversion Documentation

## Overview
This document describes the conversion of COBOL code to Java using an AI-powered conversion system.

## Original COBOL Code
```cobol
{cobol_code}
```

## Converted Java Code
```java
{java_code}
```

## Conversion Details
- **Conversion Date**: {asyncio.get_event_loop().time()}
- **Conversion Method**: AI-powered with human review
- **Complexity Analysis**: {await self.analyze_complexity(cobol_code)}

## Key Changes Made
1. Data type conversions
2. Control flow transformations
3. File handling modernization
4. Error handling improvements
5. Code structure optimization

## Testing Recommendations
- Unit tests for all converted methods
- Integration tests for file operations
- Performance testing for large datasets
- Edge case testing

## Notes
This code was automatically converted and should be reviewed by a Java developer before production use.
"""
        
        return documentation
