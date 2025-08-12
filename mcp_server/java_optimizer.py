import re
import json
from typing import Dict, Any, List, Optional
from utils import logger

class JavaOptimizer:
    def __init__(self):
        self.ready = True
        self.optimization_metrics = {
            "optimizations_applied": 0,
            "performance_improvements": 0,
            "code_quality_improvements": 0,
            "memory_optimizations": 0
        }
        
    def is_ready(self) -> bool:
        """Check if optimizer is ready"""
        return self.ready
    
    async def optimize(self, java_code: str, original_cobol: str = "") -> str:
        """Optimize Java code converted from COBOL"""
        try:
            optimized_code = java_code
            
            # Apply various optimizations
            optimized_code = await self._optimize_imports(optimized_code)
            optimized_code = await self._optimize_data_structures(optimized_code)
            optimized_code = await self._optimize_control_flow(optimized_code)
            optimized_code = await self._optimize_file_operations(optimized_code)
            optimized_code = await self._optimize_error_handling(optimized_code)
            optimized_code = await self._optimize_performance(optimized_code)
            optimized_code = await self._optimize_code_style(optimized_code)
            
            # Update metrics
            self.optimization_metrics["optimizations_applied"] += 1
            
            return optimized_code
            
        except Exception as e:
            logger.error(f"Java optimization error: {e}")
            return java_code
    
    async def _optimize_imports(self, code: str) -> str:
        """Optimize import statements"""
        lines = code.split('\n')
        optimized_lines = []
        imports = []
        other_lines = []
        
        for line in lines:
            if line.strip().startswith('import '):
                imports.append(line.strip())
            else:
                other_lines.append(line)
        
        # Remove duplicate imports
        unique_imports = list(set(imports))
        unique_imports.sort()
        
        # Add optimized imports
        optimized_lines.extend(unique_imports)
        optimized_lines.append('')  # Empty line after imports
        optimized_lines.extend(other_lines)
        
        self.optimization_metrics["code_quality_improvements"] += 1
        return '\n'.join(optimized_lines)
    
    async def _optimize_data_structures(self, code: str) -> str:
        """Optimize data structures and variable declarations"""
        # Replace inefficient data structures
        optimizations = [
            # Replace String concatenation in loops with StringBuilder
            (r'String\s+\w+\s*=\s*"";', 'StringBuilder result = new StringBuilder();'),
            (r'(\w+)\s*\+=\s*(\w+);', r'result.append(\2);'),
            
            # Use more efficient collections
            (r'Vector<', 'ArrayList<'),
            (r'Hashtable<', 'HashMap<'),
            
            # Use primitive types where possible
            (r'Integer\s+(\w+)\s*=\s*(\d+);', r'int \1 = \2;'),
            (r'Long\s+(\w+)\s*=\s*(\d+);', r'long \1 = \2;'),
        ]
        
        optimized_code = code
        for pattern, replacement in optimizations:
            optimized_code = re.sub(pattern, replacement, optimized_code)
        
        self.optimization_metrics["performance_improvements"] += 1
        return optimized_code
    
    async def _optimize_control_flow(self, code: str) -> str:
        """Optimize control flow structures"""
        # Simplify nested if statements
        code = re.sub(
            r'if\s*\(([^)]+)\)\s*\{\s*if\s*\(([^)]+)\)\s*\{',
            r'if (\1 && \2) {',
            code
        )
        
        # Use enhanced for loops where possible
        code = re.sub(
            r'for\s*\(\s*int\s+(\w+)\s*=\s*0;\s*\1\s*<\s*(\w+)\.length;\s*\1\+\+\)\s*\{',
            r'for (String item : \2) {',
            code
        )
        
        # Optimize switch statements
        code = re.sub(
            r'if\s*\((\w+)\s*==\s*(\w+)\)\s*\{([^}]+)\}\s*else\s*if\s*\((\w+)\s*==\s*(\w+)\)\s*\{([^}]+)\}',
            r'switch (\1) {\n    case \2:\n        \3\n        break;\n    case \5:\n        \6\n        break;\n}',
            code
        )
        
        self.optimization_metrics["code_quality_improvements"] += 1
        return code
    
    async def _optimize_file_operations(self, code: str) -> str:
        """Optimize file handling operations"""
        # Replace old file operations with modern Java NIO
        optimizations = [
            # Use try-with-resources
            (r'FileReader\s+(\w+)\s*=\s*new\s+FileReader\(([^)]+)\);', 
             r'try (FileReader \1 = new FileReader(\2)) {'),
            (r'BufferedReader\s+(\w+)\s*=\s*new\s+BufferedReader\(([^)]+)\);',
             r'try (BufferedReader \1 = new BufferedReader(\2)) {'),
            
            # Use Path API
            (r'new\s+File\(([^)]+)\)', r'Paths.get(\1)'),
            
            # Use Files utility methods
            (r'FileReader.*readLine\(\)', 'Files.lines(Paths.get(filename))'),
        ]
        
        optimized_code = code
        for pattern, replacement in optimizations:
            optimized_code = re.sub(pattern, replacement, optimized_code)
        
        self.optimization_metrics["performance_improvements"] += 1
        return optimized_code
    
    async def _optimize_error_handling(self, code: str) -> str:
        """Optimize error handling and exception management"""
        # Add proper exception handling
        if 'FileNotFoundException' in code and 'try' not in code:
            code = re.sub(
                r'(FileReader\s+\w+\s*=\s*new\s+FileReader\([^)]+\);)',
                r'try {\n        \1\n    } catch (FileNotFoundException e) {\n        System.err.println("File not found: " + e.getMessage());\n        return;\n    }',
                code
            )
        
        # Use specific exceptions instead of generic Exception
        code = re.sub(r'catch\s*\(\s*Exception\s+e\s*\)', 'catch (IOException e)', code)
        
        # Add logging instead of print statements
        code = re.sub(
            r'System\.out\.println\(([^)]+)\);',
            r'logger.info(\1);',
            code
        )
        code = re.sub(
            r'System\.err\.println\(([^)]+)\);',
            r'logger.error(\1);',
            code
        )
        
        self.optimization_metrics["code_quality_improvements"] += 1
        return code
    
    async def _optimize_performance(self, code: str) -> str:
        """Apply performance optimizations"""
        # Use StringBuilder for string concatenation in loops
        if re.search(r'String\s+\w+\s*\+=\s*\w+', code):
            code = re.sub(
                r'String\s+(\w+)\s*=\s*"";',
                r'StringBuilder \1 = new StringBuilder();',
                code
            )
            code = re.sub(
                r'(\w+)\s*\+=\s*(\w+);',
                r'\1.append(\2);',
                code
            )
        
        # Use ArrayList instead of Vector for better performance
        code = re.sub(r'Vector<', 'ArrayList<', code)
        
        # Use HashMap instead of Hashtable
        code = re.sub(r'Hashtable<', 'HashMap<', code)
        
        # Optimize loops
        code = re.sub(
            r'for\s*\(\s*int\s+(\w+)\s*=\s*0;\s*\1\s*<\s*(\w+)\.length;\s*\1\+\+\)',
            r'for (int \1 = 0; \1 < \2.length; \1++)',
            code
        )
        
        self.optimization_metrics["performance_improvements"] += 1
        return code
    
    async def _optimize_code_style(self, code: str) -> str:
        """Apply code style and formatting optimizations"""
        # Fix indentation
        lines = code.split('\n')
        optimized_lines = []
        indent_level = 0
        
        for line in lines:
            stripped = line.strip()
            if not stripped:
                optimized_lines.append('')
                continue
            
            # Decrease indent for closing braces
            if stripped.startswith('}'):
                indent_level = max(0, indent_level - 1)
            
            # Add proper indentation
            optimized_line = '    ' * indent_level + stripped
            optimized_lines.append(optimized_line)
            
            # Increase indent for opening braces
            if stripped.endswith('{'):
                indent_level += 1
        
        # Add proper spacing around operators
        code = re.sub(r'(\w+)([=+\-*/])(\w+)', r'\1 \2 \3', code)
        
        # Add proper spacing after keywords
        code = re.sub(r'(if|for|while|switch)\s*\(', r'\1 (', code)
        
        self.optimization_metrics["code_quality_improvements"] += 1
        return '\n'.join(optimized_lines)
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get optimization metrics"""
        return {
            "optimizations_applied": self.optimization_metrics["optimizations_applied"],
            "performance_improvements": self.optimization_metrics["performance_improvements"],
            "code_quality_improvements": self.optimization_metrics["code_quality_improvements"],
            "memory_optimizations": self.optimization_metrics["memory_optimizations"],
            "total_improvements": sum(self.optimization_metrics.values())
        }
    
    async def analyze_optimization_potential(self, java_code: str) -> Dict[str, Any]:
        """Analyze potential optimization opportunities"""
        analysis = {
            "optimization_opportunities": [],
            "estimated_improvement": 0,
            "priority_areas": []
        }
        
        # Check for common optimization opportunities
        if re.search(r'String\s+\w+\s*\+=\s*\w+', java_code):
            analysis["optimization_opportunities"].append({
                "type": "string_concatenation",
                "description": "Replace string concatenation with StringBuilder",
                "priority": "high",
                "estimated_improvement": 15
            })
        
        if re.search(r'Vector<', java_code):
            analysis["optimization_opportunities"].append({
                "type": "collection_choice",
                "description": "Replace Vector with ArrayList for better performance",
                "priority": "medium",
                "estimated_improvement": 10
            })
        
        if re.search(r'FileReader\s+\w+\s*=\s*new\s+FileReader', java_code):
            analysis["optimization_opportunities"].append({
                "type": "file_operations",
                "description": "Use try-with-resources for automatic resource management",
                "priority": "high",
                "estimated_improvement": 20
            })
        
        if re.search(r'System\.out\.println', java_code):
            analysis["optimization_opportunities"].append({
                "type": "logging",
                "description": "Replace System.out.println with proper logging",
                "priority": "medium",
                "estimated_improvement": 5
            })
        
        # Calculate total estimated improvement
        total_improvement = sum(opp["estimated_improvement"] for opp in analysis["optimization_opportunities"])
        analysis["estimated_improvement"] = min(total_improvement, 100)
        
        # Identify priority areas
        high_priority = [opp for opp in analysis["optimization_opportunities"] if opp["priority"] == "high"]
        if high_priority:
            analysis["priority_areas"].extend([opp["type"] for opp in high_priority])
        
        return analysis
    
    async def generate_optimization_report(self, original_code: str, optimized_code: str) -> str:
        """Generate a detailed optimization report"""
        analysis = await self.analyze_optimization_potential(original_code)
        metrics = self.get_metrics()
        
        report = f"""# Java Code Optimization Report

## Overview
This report details the optimizations applied to the Java code converted from COBOL.

## Optimization Metrics
- **Total Optimizations Applied**: {metrics['optimizations_applied']}
- **Performance Improvements**: {metrics['performance_improvements']}
- **Code Quality Improvements**: {metrics['code_quality_improvements']}
- **Memory Optimizations**: {metrics['memory_optimizations']}

## Optimization Opportunities Identified
"""
        
        for opportunity in analysis["optimization_opportunities"]:
            report += f"""
### {opportunity['type'].replace('_', ' ').title()}
- **Description**: {opportunity['description']}
- **Priority**: {opportunity['priority']}
- **Estimated Improvement**: {opportunity['estimated_improvement']}%
"""
        
        report += f"""
## Priority Areas
{', '.join(analysis['priority_areas'])}

## Estimated Overall Improvement
{analysis['estimated_improvement']}%

## Recommendations
1. Review all optimizations for correctness
2. Test the optimized code thoroughly
3. Monitor performance in production
4. Consider additional optimizations based on usage patterns
"""
        
        return report
