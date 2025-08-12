from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime
from enum import Enum

class ConversionStatus(str, Enum):
    PENDING = "pending"
    PLANNING = "planning"
    CONVERTING = "converting"
    REVIEWING = "reviewing"
    FIXING = "fixing"
    VERIFYING = "verifying"
    COMPLETED = "completed"
    FAILED = "failed"
    NEEDS_FEEDBACK = "needs_feedback"

class FeedbackType(str, Enum):
    APPROVAL = "approval"
    REJECTION = "rejection"
    SUGGESTION = "suggestion"
    BUG_REPORT = "bug_report"

class ConversionRequest(BaseModel):
    cobol_code: str = Field(..., description="COBOL source code to convert")
    client_id: str = Field(..., description="Unique client identifier")
    options: Optional[Dict[str, Any]] = Field(default={}, description="Conversion options")
    priority: Optional[str] = Field(default="normal", description="Processing priority")
    
class ConversionResponse(BaseModel):
    job_id: str = Field(..., description="Unique job identifier")
    status: str = Field(..., description="Current conversion status")
    message: str = Field(..., description="Status message")
    
class FeedbackRequest(BaseModel):
    job_id: str = Field(..., description="Job ID to provide feedback for")
    feedback_type: FeedbackType = Field(..., description="Type of feedback")
    message: str = Field(..., description="Feedback message")
    node_name: Optional[str] = Field(None, description="Specific workflow node")
    requires_rework: bool = Field(default=False, description="Whether feedback requires rework")
    suggested_changes: Optional[Dict[str, Any]] = Field(None, description="Suggested changes")
    
class ConversionResult(BaseModel):
    job_id: str
    status: ConversionStatus
    cobol_code: str
    java_code: str
    conversion_plan: Dict[str, Any]
    review_comments: List[str]
    fixes_applied: List[str]
    verification_results: Dict[str, Any]
    execution_time: float
    created_at: datetime
    updated_at: datetime
    
class WorkflowNode(BaseModel):
    name: str
    status: ConversionStatus
    input_data: Dict[str, Any]
    output_data: Dict[str, Any]
    execution_time: float
    error_message: Optional[str] = None
    
class ConversionHistory(BaseModel):
    job_id: str
    status: ConversionStatus
    created_at: datetime
    completed_at: Optional[datetime] = None
    execution_time: Optional[float] = None
    error_message: Optional[str] = None
    
class BatchRequest(BaseModel):
    conversions: List[ConversionRequest]
    batch_options: Optional[Dict[str, Any]] = Field(default={})
    
class BatchResponse(BaseModel):
    batch_id: str
    job_ids: List[str]
    status: str
    message: str
    
class WebSocketMessage(BaseModel):
    type: str
    job_id: str
    data: Dict[str, Any]
    timestamp: datetime = Field(default_factory=datetime.now)
    
class MCPRequest(BaseModel):
    method: str
    params: Dict[str, Any]
    
class MCPResponse(BaseModel):
    result: Dict[str, Any]
    error: Optional[str] = None
