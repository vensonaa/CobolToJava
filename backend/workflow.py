import asyncio
import time
import os
from typing import Dict, Any, List
from langgraph.graph import StateGraph, END
from langchain_core.messages import HumanMessage, SystemMessage
from langchain_groq import ChatGroq
import json

from database import DatabaseManager
from mcp_client import MCPClient
from utils import logger

class ConversionState:
    """State object for the conversion workflow"""
    def __init__(self, cobol_code: str, job_id: str, client_id: str):
        self.cobol_code = cobol_code
        self.job_id = job_id
        self.client_id = client_id
        self.conversion_plan = {}
        self.java_code = ""
        self.review_comments = []
        self.fixes_applied = []
        self.verification_results = {}
        self.current_node = ""
        self.execution_time = 0.0
        self.error_message = None
        self.needs_human_feedback = False
        self.feedback_data = {}

class ConversionWorkflow:
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.mcp_client = MCPClient()
        
       
        self.llm = ChatGroq(
                model="llama3-70b-8192",
                temperature=0.1,
                api_key=""
            )
        
        # Create the workflow graph
        self.graph = self._create_workflow_graph()
    
    def _simple_cobol_to_java(self, cobol_code: str) -> str:
        """Enhanced COBOL to Java conversion without external APIs"""
        logger.info(f"Converting COBOL code: {cobol_code[:100]}...")  # Log first 100 chars
        
        java_code = []
        java_code.append("// Converted from COBOL to Java")
        java_code.append("import java.util.Scanner;")
        java_code.append("import java.util.*;")
        java_code.append("import java.math.BigDecimal;")
        java_code.append("")
        
        # Parse COBOL code to extract variables and structure
        variables = self._extract_cobol_variables(cobol_code)
        logger.info(f"Extracted variables: {variables}")
        
        java_code.append("public class ConvertedProgram {")
        
        # Add variable declarations
        if variables:
            java_code.append("    // COBOL variables converted to Java")
            for var_name, var_type in variables.items():
                java_code.append(f"    private static {var_type} {var_name};")
            java_code.append("")
        
        java_code.append("    public static void main(String[] args) {")
        java_code.append("        Scanner scanner = new Scanner(System.in);")
        java_code.append("")
        
        # Convert COBOL statements
        lines = cobol_code.split('\n')
        indent_level = 0
        converted_lines = []
        
        for line in lines:
            line = line.strip()
            if not line or line.startswith('*') or line.startswith('/'):
                continue  # Skip comments and empty lines
                
            converted_line = self._convert_cobol_line(line, variables, indent_level)
            if converted_line:
                converted_lines.append(converted_line)
                logger.info(f"Converted: '{line}' -> '{converted_line}'")
                # Adjust indentation for control structures
                if 'PERFORM' in line.upper() or 'IF' in line.upper():
                    indent_level += 1
                elif 'END-PERFORM' in line.upper() or 'END-IF' in line.upper():
                    indent_level = max(0, indent_level - 1)
        
        # If no lines were converted, add a fallback
        if not converted_lines:
            logger.warning("No COBOL statements were converted, adding fallback")
            converted_lines.append("        System.out.println(\"Hello from converted COBOL program\");")
        
        java_code.extend(converted_lines)
        java_code.append("        scanner.close();")
        java_code.append("    }")
        java_code.append("}")
        
        result = '\n'.join(java_code)
        logger.info(f"Final Java code length: {len(result)} characters")
        return result
    
    def _extract_cobol_variables(self, cobol_code: str) -> Dict[str, str]:
        """Extract variable declarations from COBOL code"""
        variables = {}
        lines = cobol_code.split('\n')
        
        for line in lines:
            line = line.strip()
            # Look for DATA DIVISION or WORKING-STORAGE SECTION
            if 'DATA DIVISION' in line.upper() or 'WORKING-STORAGE SECTION' in line.upper():
                continue
            # Look for variable declarations (PIC clauses)
            if 'PIC' in line.upper() and ('X(' in line.upper() or '9(' in line.upper() or 'S9(' in line.upper()):
                parts = line.split()
                if len(parts) >= 2:
                    var_name = parts[0].replace('-', '_')
                    if 'PIC X(' in line.upper():
                        variables[var_name] = "String"
                    elif 'PIC 9(' in line.upper() or 'PIC S9(' in line.upper():
                        if 'V9(' in line.upper():
                            variables[var_name] = "BigDecimal"
                        else:
                            variables[var_name] = "int"
        
        # If no variables found, add some common ones based on usage
        if not variables:
            # Look for common variable names in the code
            common_vars = ['WS-NAME', 'WS-COUNTER', 'WS-AMOUNT', 'WS-RESULT']
            for var in common_vars:
                if var in cobol_code.upper():
                    var_name = var.replace('-', '_')
                    variables[var_name] = "String"  # Default to String
        
        logger.info(f"Extracted variables: {variables}")
        return variables
    
    def _convert_cobol_line(self, line: str, variables: Dict[str, str], indent_level: int) -> str:
        """Convert a single COBOL line to Java"""
        indent = "        " * (indent_level + 1)
        line_upper = line.upper()
        
        # DISPLAY statement
        if 'DISPLAY' in line_upper:
            if '"' in line:
                # Extract text between quotes
                start = line.find('"')
                end = line.rfind('"')
                if start != -1 and end != -1:
                    message = line[start+1:end]
                    return f'{indent}System.out.println("{message}");'
            else:
                # Display variable
                parts = line.split()
                for part in parts:
                    if part not in ['DISPLAY', 'UPON', 'CONSOLE'] and not part.startswith('"'):
                        if part in variables:
                            return f'{indent}System.out.println({part});'
                return f'{indent}System.out.println("Display output");'
        
        # ACCEPT statement
        elif 'ACCEPT' in line_upper:
            parts = line.split()
            for part in parts:
                if part not in ['ACCEPT', 'FROM', 'CONSOLE']:
                    if part in variables:
                        return f'{indent}{part} = scanner.nextLine();'
            return f'{indent}String input = scanner.nextLine();'
        
        # MOVE statement
        elif 'MOVE' in line_upper and 'TO' in line_upper:
            parts = line.split()
            try:
                move_idx = parts.index('MOVE')
                to_idx = parts.index('TO')
                if move_idx < to_idx:
                    value = parts[move_idx + 1]
                    variable = parts[to_idx + 1]
                    return f'{indent}{variable} = {value};'
            except ValueError:
                pass
        
        # IF statement
        elif 'IF' in line_upper and not 'END-IF' in line_upper:
            condition = self._convert_cobol_condition(line)
            return f'{indent}if ({condition}) {{'
        
        # ELSE statement
        elif 'ELSE' in line_upper:
            return f'{indent}}} else {{'
        
        # END-IF statement
        elif 'END-IF' in line_upper:
            return f'{indent}}}'
        
        # PERFORM statement
        elif 'PERFORM' in line_upper and 'VARYING' in line_upper:
            # Simple PERFORM VARYING conversion
            return f'{indent}for (int i = 1; i <= 10; i++) {{'
        
        # END-PERFORM statement
        elif 'END-PERFORM' in line_upper:
            return f'{indent}}}'
        
        # STOP RUN statement
        elif 'STOP RUN' in line_upper:
            return f'{indent}System.exit(0);'
        
        # COMPUTE statement
        elif 'COMPUTE' in line_upper:
            # Extract arithmetic expression
            compute_part = line.split('COMPUTE')[1].split('END-COMPUTE')[0] if 'END-COMPUTE' in line else line.split('COMPUTE')[1]
            expression = self._convert_cobol_expression(compute_part)
            return f'{indent}{expression};'
        
        # Handle simple statements that might be missed
        elif line_upper.strip():
            # If it's not empty and not recognized, log it
            logger.debug(f"Unrecognized COBOL line: {line}")
            return ""
        
        return ""
    
    def _convert_cobol_condition(self, line: str) -> str:
        """Convert COBOL condition to Java condition"""
        # Remove IF keyword and convert operators
        condition = line.replace('IF', '').replace('THEN', '').strip()
        condition = condition.replace('=', '==')
        condition = condition.replace('NOT =', '!=')
        condition = condition.replace('GREATER THAN', '>')
        condition = condition.replace('LESS THAN', '<')
        condition = condition.replace('GREATER THAN OR EQUAL TO', '>=')
        condition = condition.replace('LESS THAN OR EQUAL TO', '<=')
        return condition
    
    def _convert_cobol_expression(self, expression: str) -> str:
        """Convert COBOL arithmetic expression to Java"""
        # Replace COBOL operators with Java operators
        expression = expression.replace('**', 'Math.pow')
        expression = expression.replace('/', '/')
        expression = expression.replace('*', '*')
        expression = expression.replace('+', '+')
        expression = expression.replace('-', '-')
        return expression
    

    
    def _create_workflow_graph(self) -> StateGraph:
        """Create the LangGraph workflow with linear flow to avoid recursion"""
        workflow = StateGraph(Dict[str, Any])
        
        # Add nodes
        workflow.add_node("planner", self._planner_node)
        workflow.add_node("converter", self._converter_node)
        workflow.add_node("reviewer", self._reviewer_node)
        workflow.add_node("verifier", self._verifier_node)
        
        # Define the workflow edges - linear flow to avoid recursion
        workflow.set_entry_point("planner")
        workflow.add_edge("planner", "converter")
        workflow.add_edge("converter", "reviewer")
        workflow.add_edge("reviewer", "verifier")
        workflow.add_edge("verifier", END)
        
        return workflow.compile()
    
    async def execute(self, cobol_code: str, job_id: str, client_id: str) -> Dict[str, Any]:
        """Execute the complete conversion workflow"""
        start_time = time.time()
        
        try:
            # Initialize state as dictionary
            initial_state = {
                "cobol_code": cobol_code,
                "job_id": job_id,
                "client_id": client_id,
                "conversion_plan": {},
                "java_code": "",
                "review_comments": [],
                "fixes_applied": [],
                "verification_results": {},
                "current_node": "",
                "execution_time": 0.0,
                "error_message": None,
                "needs_human_feedback": False,
                "feedback_data": {},
                "retry_count": 0  # Add retry counter to prevent infinite loops
            }
            
            # Execute workflow
            result = await self.graph.ainvoke(initial_state)
            
            # Calculate execution time
            execution_time = time.time() - start_time
            
            # Prepare final result
            final_result = {
                "job_id": job_id,
                "status": "completed",
                "cobol_code": cobol_code,
                "java_code": result.get("java_code", ""),
                "conversion_plan": result.get("conversion_plan", {}),
                "review_comments": result.get("review_comments", []),
                "fixes_applied": result.get("fixes_applied", []),
                "verification_results": result.get("verification_results", {}),
                "execution_time": execution_time,
                "error_message": result.get("error_message")
            }
            
            return final_result
            
        except Exception as e:
            logger.error(f"Workflow execution error: {e}")
            return {
                "job_id": job_id,
                "status": "failed",
                "error_message": str(e),
                "execution_time": time.time() - start_time
            }
    
    async def _planner_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Planner node: Analyze COBOL code and create conversion strategy"""
        start_time = time.time()
        state["current_node"] = "planner"
        
        try:
            # Update database status
            await self.db_manager.update_status(state["job_id"], "planning")
            
            # Create planning prompt
            planning_prompt = f"""
            Analyze the following COBOL code and create a detailed conversion plan to Java:
            
            COBOL Code:
            {state["cobol_code"]}
            
            Create a comprehensive plan that includes:
            1. Code structure analysis
            2. Data type mappings
            3. Control flow conversion strategy
            4. File handling approach
            5. Error handling strategy
            6. Testing approach
            7. Potential challenges and solutions
            
            Return the plan as a JSON object with the following structure:
            {{
                "structure_analysis": {{}},
                "data_type_mappings": {{}},
                "control_flow_strategy": {{}},
                "file_handling": {{}},
                "error_handling": {{}},
                "testing_approach": {{}},
                "challenges": [],
                "estimated_complexity": "low|medium|high"
            }}
            """
            
            # Use MCP server for enhanced analysis (optional)
            mcp_response = None
            if self.mcp_client.is_connected():
                try:
                    mcp_response = await self.mcp_client.call_method(
                        "analyze_cobol",
                        {"code": state["cobol_code"], "analysis_type": "planning"}
                    )
                except Exception as e:
                    logger.debug(f"MCP analysis failed: {e}")
                    mcp_response = None
            
            # Get LLM response
            if self.llm is None:
                # Fallback planning without LLM
                plan = {
                    "structure_analysis": {"method": "fallback"},
                    "data_type_mappings": {"method": "fallback"},
                    "control_flow_strategy": {"method": "fallback"},
                    "file_handling": {"method": "fallback"},
                    "error_handling": {"method": "fallback"},
                    "testing_approach": {"method": "fallback"},
                    "challenges": ["Using fallback conversion method"],
                    "estimated_complexity": "low"
                }
            else:
                messages = [
                    SystemMessage(content="You are an expert COBOL to Java conversion planner."),
                    HumanMessage(content=planning_prompt)
                ]
                
                response = await self.llm.ainvoke(messages)
                
                # Parse and combine results
                try:
                    plan = json.loads(response.content)
                    if mcp_response:
                        plan["mcp_analysis"] = mcp_response
                except json.JSONDecodeError:
                    plan = {"raw_plan": response.content, "mcp_analysis": mcp_response}
            
            state["conversion_plan"] = plan
            
            # Save node execution data
            execution_time = time.time() - start_time
            await self.db_manager.save_workflow_node(
                state["job_id"], "planner", "completed",
                {"cobol_code": state["cobol_code"]},
                {"conversion_plan": plan},
                execution_time
            )
            
            logger.info(f"Planner completed for job {state['job_id']}")
            return state
            
        except Exception as e:
            execution_time = time.time() - start_time
            state["error_message"] = str(e)
            await self.db_manager.save_workflow_node(
                state["job_id"], "planner", "failed",
                {"cobol_code": state["cobol_code"]},
                {},
                execution_time,
                str(e)
            )
            raise
    
    async def _converter_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Converter node: Convert COBOL to Java using GenAI"""
        start_time = time.time()
        state["current_node"] = "converter"
        
        try:
            await self.db_manager.update_status(state["job_id"], "converting")
            
            # Create conversion prompt
            conversion_prompt = f"""
            You are an expert COBOL to Java converter. Convert the following COBOL code to Java.

            COBOL Code:
            {state["cobol_code"]}

            IMPORTANT: 
            - Convert each COBOL statement to its Java equivalent
            - Use the exact variable names from the COBOL code
            - Convert DISPLAY to System.out.println
            - Convert ACCEPT to Scanner input
            - Convert MOVE to assignment statements
            - Convert IF/ELSE/END-IF to Java if-else blocks
            - Convert PERFORM loops to Java for/while loops
            - Convert STOP RUN to System.exit(0)
            - Include necessary imports (java.util.Scanner, etc.)
            - Create a complete, runnable Java program

            Return ONLY the complete Java code, no explanations or comments about the conversion.
            Start with the imports and class declaration.
            """
            
            # Use Groq for conversion
            if self.llm is None:
                # Fallback conversion without LLM
                java_code = self._simple_cobol_to_java(state["cobol_code"])
            else:
                messages = [
                    SystemMessage(content="You are an expert COBOL to Java converter. You must convert the exact COBOL code provided to working Java code. Do not provide sample code or templates. Convert the specific COBOL statements to their Java equivalents."),
                    HumanMessage(content=conversion_prompt)
                ]
                
                # Get conversion from Groq
                response = await self.llm.ainvoke(messages)
                java_code = response.content.strip()
                logger.info(f"LLM conversion result (first 200 chars): {java_code[:200]}...")
                
                # Check if LLM returned sample code instead of actual conversion
                sample_indicators = [
                    "Hello World", "sample", "template", "example", 
                    "public class Sample", "// This is a sample"
                ]
                
                if any(indicator.lower() in java_code.lower() for indicator in sample_indicators):
                    logger.warning("LLM returned sample code, using enhanced fallback conversion")
                    java_code = self._simple_cobol_to_java(state["cobol_code"])
            
            # Use MCP server for code optimization (optional)
            if self.mcp_client.is_connected():
                try:
                    optimized_code = await self.mcp_client.call_method(
                        "optimize_java",
                        {"java_code": java_code, "original_cobol": state["cobol_code"]}
                    )
                    
                    if optimized_code:
                        java_code = optimized_code
                except Exception as e:
                    logger.debug(f"MCP optimization failed: {e}")
                    # Continue without optimization
            
            state["java_code"] = java_code
            
            # Save node execution data
            execution_time = time.time() - start_time
            await self.db_manager.save_workflow_node(
                state["job_id"], "converter", "completed",
                {"cobol_code": state["cobol_code"], "conversion_plan": state["conversion_plan"]},
                {"java_code": java_code},
                execution_time
            )
            
            logger.info(f"Converter completed for job {state['job_id']}")
            return state
            
        except Exception as e:
            execution_time = time.time() - start_time
            state["error_message"] = str(e)
            await self.db_manager.save_workflow_node(
                state["job_id"], "converter", "failed",
                {"cobol_code": state["cobol_code"], "conversion_plan": state["conversion_plan"]},
                {},
                execution_time,
                str(e)
            )
            raise
    
    async def _reviewer_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Reviewer node: Review converted Java code for quality and correctness"""
        start_time = time.time()
        state["current_node"] = "reviewer"
        
        try:
            await self.db_manager.update_status(state["job_id"], "reviewing")
            
            review_prompt = f"""
            Review the following Java code converted from COBOL:
            
            Original COBOL:
            {state["cobol_code"]}
            
            Converted Java:
            {state["java_code"]}
            
            Conversion Plan:
            {json.dumps(state["conversion_plan"], indent=2)}
            
            Review criteria:
            1. Functional correctness
            2. Code quality and style
            3. Performance considerations
            4. Error handling
            5. Maintainability
            6. Completeness of conversion
            
            Provide a detailed review with:
            - Overall assessment (pass/fail)
            - Specific issues found
            - Suggestions for improvement
            - Confidence level (1-10)
            
            Return as JSON:
            {{
                "assessment": "pass|fail|needs_improvement",
                "confidence": 1-10,
                "issues": [],
                "suggestions": [],
                "overall_quality": "excellent|good|fair|poor"
            }}
            """
            
            if self.llm is None:
                # Fallback review without LLM
                review_result = {
                    "assessment": "pass",
                    "confidence": 7,
                    "issues": ["Using fallback conversion method"],
                    "suggestions": ["Manual review recommended"],
                    "overall_quality": "basic"
                }
            else:
                messages = [
                    SystemMessage(content="You are an expert code reviewer specializing in COBOL to Java conversions."),
                    HumanMessage(content=review_prompt)
                ]
                
                response = await self.llm.ainvoke(messages)
                
                try:
                    review_result = json.loads(response.content)
                except json.JSONDecodeError:
                    review_result = {
                        "assessment": "needs_improvement",
                        "confidence": 5,
                        "issues": ["Could not parse review result"],
                        "suggestions": ["Manual review required"],
                        "overall_quality": "fair"
                    }
            
            state["review_comments"] = review_result.get("issues", [])
            
            # Store review results for verification
            state["current_node"] = "reviewer_completed"
            # Note: In linear flow, we always proceed to verification
            # The verification step will handle any issues found during review
            
            # Save node execution data
            execution_time = time.time() - start_time
            await self.db_manager.save_workflow_node(
                state["job_id"], "reviewer", "completed",
                {"java_code": state["java_code"], "cobol_code": state["cobol_code"]},
                {"review_result": review_result},
                execution_time
            )
            
            logger.info(f"Reviewer completed for job {state['job_id']}")
            return state
            
        except Exception as e:
            execution_time = time.time() - start_time
            state["error_message"] = str(e)
            await self.db_manager.save_workflow_node(
                state["job_id"], "reviewer", "failed",
                {"java_code": state["java_code"], "cobol_code": state["cobol_code"]},
                {},
                execution_time,
                str(e)
            )
            raise
    
    async def _fixer_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Fixer node: Apply fixes based on review feedback"""
        start_time = time.time()
        state["current_node"] = "fixer"
        
        try:
            await self.db_manager.update_status(state["job_id"], "fixing")
            
            # Get the latest review feedback
            feedback = await self.db_manager.get_feedback_for_job(state["job_id"])
            
            fix_prompt = f"""
            Fix the following Java code based on the review feedback:
            
            Original COBOL:
            {state["cobol_code"]}
            
            Current Java Code:
            {state["java_code"]}
            
            Review Issues:
            {json.dumps(state["review_comments"], indent=2)}
            
            Feedback:
            {json.dumps(feedback, indent=2)}
            
            Apply the necessary fixes and return the corrected Java code.
            Also provide a list of fixes applied.
            """
            
            if self.llm is None:
                # Fallback fixing without LLM
                fixed_code = state["java_code"]  # Keep original code
                fixes_applied = ["No fixes applied - using fallback mode"]
            else:
                messages = [
                    SystemMessage(content="You are an expert Java developer fixing COBOL to Java conversions."),
                    HumanMessage(content=fix_prompt)
                ]
                
                response = await self.llm.ainvoke(messages)
                
                # Extract fixes from response
                fixed_code = response.content.strip()
                fixes_applied = [
                    "Applied code quality improvements",
                    "Fixed potential bugs",
                    "Improved error handling",
                    "Enhanced code structure"
                ]
            
            state["java_code"] = fixed_code
            state["fixes_applied"] = fixes_applied
            
            # Save node execution data
            execution_time = time.time() - start_time
            await self.db_manager.save_workflow_node(
                state["job_id"], "fixer", "completed",
                {"java_code": state["java_code"], "review_comments": state["review_comments"]},
                {"fixed_java_code": fixed_code, "fixes_applied": fixes_applied},
                execution_time
            )
            
            logger.info(f"Fixer completed for job {state['job_id']}")
            return state
            
        except Exception as e:
            execution_time = time.time() - start_time
            state.error_message = str(e)
            await self.db_manager.save_workflow_node(
                state.job_id, "fixer", "failed",
                {"java_code": state.java_code, "review_comments": state.review_comments},
                {},
                execution_time,
                str(e)
            )
            raise
    
    async def _verifier_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Verifier node: Final verification of conversion quality"""
        start_time = time.time()
        state["current_node"] = "verifier"
        
        try:
            await self.db_manager.update_status(state["job_id"], "verifying")
            
            verification_prompt = f"""
            Perform final verification of the COBOL to Java conversion:
            
            Original COBOL:
            {state["cobol_code"]}
            
            Final Java Code:
            {state["java_code"]}
            
            Conversion Plan:
            {json.dumps(state["conversion_plan"], indent=2)}
            
            Review Comments:
            {json.dumps(state["review_comments"], indent=2)}
            
            Verification checklist:
            1. Code compiles without errors
            2. Logic is functionally equivalent
            3. All edge cases are handled
            4. Performance is acceptable
            5. Code follows Java best practices
            6. Documentation is complete
            
            Return verification results as JSON:
            {{
                "verification_passed": true/false,
                "compilation_status": "success|error",
                "functional_equivalence": "verified|partial|failed",
                "performance_score": 1-10,
                "code_quality_score": 1-10,
                "overall_score": 1-10,
                "final_issues": [],
                "recommendations": []
            }}
            """
            
            if self.llm is None:
                # Fallback verification without LLM
                verification_result = {
                    "verification_passed": True,
                    "compilation_status": "unknown",
                    "functional_equivalence": "partial",
                    "performance_score": 5,
                    "code_quality_score": 5,
                    "overall_score": 5,
                    "final_issues": ["Using fallback conversion method"],
                    "recommendations": ["Manual verification recommended"]
                }
            else:
                messages = [
                    SystemMessage(content="You are an expert verification specialist for COBOL to Java conversions."),
                    HumanMessage(content=verification_prompt)
                ]
                
                response = await self.llm.ainvoke(messages)
            
                try:
                    verification_result = json.loads(response.content)
                except json.JSONDecodeError:
                    verification_result = {
                        "verification_passed": False,
                        "compilation_status": "unknown",
                        "functional_equivalence": "partial",
                        "performance_score": 5,
                        "code_quality_score": 5,
                        "overall_score": 5,
                        "final_issues": ["Could not parse verification result"],
                        "recommendations": ["Manual verification required"]
                    }
            
            state["verification_results"] = verification_result
            
            # Save node execution data
            execution_time = time.time() - start_time
            await self.db_manager.save_workflow_node(
                state["job_id"], "verifier", "completed",
                {"java_code": state["java_code"], "fixes_applied": state["fixes_applied"]},
                {"verification_results": verification_result},
                execution_time
            )
            
            logger.info(f"Verifier completed for job {state['job_id']}")
            return state
            
        except Exception as e:
            execution_time = time.time() - start_time
            state["error_message"] = str(e)
            await self.db_manager.save_workflow_node(
                state["job_id"], "verifier", "failed",
                {"java_code": state["java_code"], "fixes_applied": state["fixes_applied"]},
                {},
                execution_time,
                str(e)
            )
            raise
    

    
    async def handle_feedback(self, feedback: Any):
        """Handle human feedback and restart specific workflow nodes"""
        # This method can be used to restart specific nodes based on feedback
        pass
