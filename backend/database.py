import aiosqlite
import json
from datetime import datetime
from typing import List, Dict, Any, Optional
from models import ConversionRequest, ConversionResult, FeedbackRequest, ConversionHistory
from utils import logger

class DatabaseManager:
    def __init__(self, db_path: str = "cobol_converter.db"):
        self.db_path = db_path
        
    async def initialize(self):
        """Initialize database tables"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                CREATE TABLE IF NOT EXISTS conversions (
                    job_id TEXT PRIMARY KEY,
                    status TEXT NOT NULL,
                    cobol_code TEXT NOT NULL,
                    java_code TEXT,
                    conversion_plan TEXT,
                    review_comments TEXT,
                    fixes_applied TEXT,
                    verification_results TEXT,
                    execution_time REAL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    client_id TEXT NOT NULL,
                    options TEXT
                )
            """)
            
            await db.execute("""
                CREATE TABLE IF NOT EXISTS workflow_nodes (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    job_id TEXT NOT NULL,
                    node_name TEXT NOT NULL,
                    status TEXT NOT NULL,
                    input_data TEXT,
                    output_data TEXT,
                    execution_time REAL,
                    error_message TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (job_id) REFERENCES conversions (job_id)
                )
            """)
            
            await db.execute("""
                CREATE TABLE IF NOT EXISTS feedback (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    job_id TEXT NOT NULL,
                    feedback_type TEXT NOT NULL,
                    message TEXT NOT NULL,
                    node_name TEXT,
                    requires_rework BOOLEAN DEFAULT FALSE,
                    suggested_changes TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (job_id) REFERENCES conversions (job_id)
                )
            """)
            
            await db.execute("""
                CREATE TABLE IF NOT EXISTS batch_jobs (
                    batch_id TEXT PRIMARY KEY,
                    status TEXT NOT NULL,
                    total_jobs INTEGER NOT NULL,
                    completed_jobs INTEGER DEFAULT 0,
                    failed_jobs INTEGER DEFAULT 0,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    completed_at TIMESTAMP
                )
            """)
            
            await db.commit()
            logger.info("Database initialized successfully")
    
    async def save_conversion_request(self, job_id: str, request: ConversionRequest):
        """Save initial conversion request"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                INSERT INTO conversions (job_id, status, cobol_code, client_id, options)
                VALUES (?, ?, ?, ?, ?)
            """, (
                job_id,
                "pending",
                request.cobol_code,
                request.client_id,
                json.dumps(request.options or {})
            ))
            await db.commit()
    
    async def update_status(self, job_id: str, status: str):
        """Update conversion status"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                UPDATE conversions 
                SET status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE job_id = ?
            """, (status, job_id))
            await db.commit()
    
    async def save_workflow_node(self, job_id: str, node_name: str, status: str, 
                               input_data: Dict[str, Any], output_data: Dict[str, Any],
                               execution_time: float, error_message: Optional[str] = None):
        """Save workflow node execution data"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                INSERT INTO workflow_nodes 
                (job_id, node_name, status, input_data, output_data, execution_time, error_message)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (
                job_id,
                node_name,
                status,
                json.dumps(input_data),
                json.dumps(output_data),
                execution_time,
                error_message
            ))
            await db.commit()
    
    async def save_conversion_result(self, job_id: str, result: Dict[str, Any]):
        """Save final conversion result"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                UPDATE conversions 
                SET java_code = ?, conversion_plan = ?, review_comments = ?,
                    fixes_applied = ?, verification_results = ?, execution_time = ?,
                    status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE job_id = ?
            """, (
                result.get("java_code", ""),
                json.dumps(result.get("conversion_plan", {})),
                json.dumps(result.get("review_comments", [])),
                json.dumps(result.get("fixes_applied", [])),
                json.dumps(result.get("verification_results", {})),
                result.get("execution_time", 0.0),
                result.get("status", "completed"),
                job_id
            ))
            await db.commit()
    
    async def save_feedback(self, job_id: str, feedback: FeedbackRequest):
        """Save human feedback"""
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                INSERT INTO feedback 
                (job_id, feedback_type, message, node_name, requires_rework, suggested_changes)
                VALUES (?, ?, ?, ?, ?, ?)
            """, (
                job_id,
                feedback.feedback_type.value,
                feedback.message,
                feedback.node_name,
                feedback.requires_rework,
                json.dumps(feedback.suggested_changes or {})
            ))
            await db.commit()
    
    async def get_status(self, job_id: str) -> Dict[str, Any]:
        """Get conversion status and details"""
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute("""
                SELECT * FROM conversions WHERE job_id = ?
            """, (job_id,))
            row = await cursor.fetchone()
            
            if not row:
                raise ValueError(f"Job {job_id} not found")
            
            # Get workflow nodes
            cursor = await db.execute("""
                SELECT * FROM workflow_nodes WHERE job_id = ? ORDER BY created_at
            """, (job_id,))
            nodes = await cursor.fetchall()
            
            # Get feedback
            cursor = await db.execute("""
                SELECT * FROM feedback WHERE job_id = ? ORDER BY created_at
            """, (job_id,))
            feedback = await cursor.fetchall()
            
            return {
                "job_id": row["job_id"],
                "status": row["status"],
                "cobol_code": row["cobol_code"],
                "java_code": row["java_code"],
                "conversion_plan": json.loads(row["conversion_plan"]) if row["conversion_plan"] else {},
                "review_comments": json.loads(row["review_comments"]) if row["review_comments"] else [],
                "fixes_applied": json.loads(row["fixes_applied"]) if row["fixes_applied"] else [],
                "verification_results": json.loads(row["verification_results"]) if row["verification_results"] else {},
                "execution_time": row["execution_time"],
                "created_at": row["created_at"],
                "updated_at": row["updated_at"],
                "client_id": row["client_id"],
                "options": json.loads(row["options"]) if row["options"] else {},
                "workflow_nodes": [
                    {
                        "node_name": node["node_name"],
                        "status": node["status"],
                        "input_data": json.loads(node["input_data"]) if node["input_data"] else {},
                        "output_data": json.loads(node["output_data"]) if node["output_data"] else {},
                        "execution_time": node["execution_time"],
                        "error_message": node["error_message"],
                        "created_at": node["created_at"]
                    }
                    for node in nodes
                ],
                "feedback": [
                    {
                        "feedback_type": fb["feedback_type"],
                        "message": fb["message"],
                        "node_name": fb["node_name"],
                        "requires_rework": fb["requires_rework"],
                        "suggested_changes": json.loads(fb["suggested_changes"]) if fb["suggested_changes"] else {},
                        "created_at": fb["created_at"]
                    }
                    for fb in feedback
                ]
            }
    
    async def get_history(self, limit: int = 50, offset: int = 0) -> List[Dict[str, Any]]:
        """Get conversion history"""
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute("""
                SELECT job_id, status, created_at, updated_at, execution_time, client_id
                FROM conversions 
                ORDER BY created_at DESC 
                LIMIT ? OFFSET ?
            """, (limit, offset))
            rows = await cursor.fetchall()
            
            return [
                {
                    "job_id": row["job_id"],
                    "status": row["status"],
                    "created_at": row["created_at"],
                    "updated_at": row["updated_at"],
                    "execution_time": row["execution_time"],
                    "client_id": row["client_id"]
                }
                for row in rows
            ]
    
    async def get_feedback_for_job(self, job_id: str) -> List[Dict[str, Any]]:
        """Get all feedback for a specific job"""
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute("""
                SELECT * FROM feedback WHERE job_id = ? ORDER BY created_at
            """, (job_id,))
            rows = await cursor.fetchall()
            
            return [
                {
                    "feedback_type": row["feedback_type"],
                    "message": row["message"],
                    "node_name": row["node_name"],
                    "requires_rework": row["requires_rework"],
                    "suggested_changes": json.loads(row["suggested_changes"]) if row["suggested_changes"] else {},
                    "created_at": row["created_at"]
                }
                for row in rows
            ]
    
    async def get_workflow_nodes(self, job_id: str) -> List[Dict[str, Any]]:
        """Get all workflow nodes for a job"""
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute("""
                SELECT * FROM workflow_nodes WHERE job_id = ? ORDER BY created_at
            """, (job_id,))
            rows = await cursor.fetchall()
            
            return [
                {
                    "node_name": row["node_name"],
                    "status": row["status"],
                    "input_data": json.loads(row["input_data"]) if row["input_data"] else {},
                    "output_data": json.loads(row["output_data"]) if row["output_data"] else {},
                    "execution_time": row["execution_time"],
                    "error_message": row["error_message"],
                    "created_at": row["created_at"]
                }
                for row in rows
            ]
