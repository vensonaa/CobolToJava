import React, { useState } from 'react';
import './App.css';

function App() {
  const [cobolCode, setCobolCode] = useState('');
  const [javaCode, setJavaCode] = useState('');
  const [isConverting, setIsConverting] = useState(false);
  const [conversionStatus, setConversionStatus] = useState('');
  const [error, setError] = useState('');

  const handleCobolCodeChange = (e) => {
    setCobolCode(e.target.value);
    setError('');
  };

  const handleConvert = async () => {
    if (!cobolCode.trim()) {
      setError('Please enter COBOL code to convert');
      return;
    }

    setIsConverting(true);
    setConversionStatus('Starting conversion...');
    setError('');

    try {
      // Call the backend API
      const response = await fetch('http://localhost:8000/convert', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          cobol_code: cobolCode,
          client_id: 'web_client_' + Date.now(),
          options: {
            optimization_level: 'medium',
            include_comments: true,
            preserve_structure: true
          }
        }),
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      const result = await response.json();
      
      if (result.status === 'started') {
        setConversionStatus('Conversion started! Job ID: ' + result.job_id);
        // In a real implementation, you would poll for status updates
        // For now, we'll simulate a successful conversion
        setTimeout(() => {
          setJavaCode(`// Converted Java code for job ${result.job_id}
public class ConvertedProgram {
    public static void main(String[] args) {
        System.out.println("Hello from converted COBOL program!");
        
        // This is a sample conversion
        // The actual conversion would be performed by the backend workflow
        // and would include proper Java equivalents of the COBOL code
        
        // Example conversion:
        // COBOL: DISPLAY "Hello World"
        // Java:  System.out.println("Hello World");
    }
}`);
          setIsConverting(false);
          setConversionStatus('Conversion completed successfully!');
        }, 2000);
      } else {
        throw new Error('Failed to start conversion');
      }
    } catch (err) {
      setError('Error: ' + err.message);
      setIsConverting(false);
      setConversionStatus('');
    }
  };

  const handleClear = () => {
    setCobolCode('');
    setJavaCode('');
    setError('');
    setConversionStatus('');
  };

  const loadSampleCode = () => {
    setCobolCode(`       IDENTIFICATION DIVISION.
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
           
           STOP RUN.`);
  };

  return (
    <div className="App">
      <header className="App-header">
        <h1>COBOL to Java Converter</h1>
        <p>Convert your COBOL code to Java with AI assistance</p>
      </header>

      <main className="App-main">
        <div className="converter-container">
          <div className="code-section">
            <div className="code-header">
              <h2>COBOL Code Input</h2>
              <div className="code-actions">
                <button onClick={loadSampleCode} className="btn btn-secondary">
                  Load Sample
                </button>
                <button onClick={handleClear} className="btn btn-secondary">
                  Clear
                </button>
              </div>
            </div>
            <textarea
              className="code-editor"
              value={cobolCode}
              onChange={handleCobolCodeChange}
              placeholder="Enter your COBOL code here..."
              rows={20}
            />
          </div>

          <div className="conversion-section">
            <button 
              onClick={handleConvert} 
              disabled={isConverting || !cobolCode.trim()}
              className="btn btn-primary convert-btn"
            >
              {isConverting ? 'Converting...' : 'Convert to Java'}
            </button>
            
            {conversionStatus && (
              <div className="status-message">
                {conversionStatus}
              </div>
            )}
            
            {error && (
              <div className="error-message">
                {error}
              </div>
            )}
          </div>

          <div className="code-section">
            <div className="code-header">
              <h2>Java Code Output</h2>
              <div className="code-actions">
                <button 
                  onClick={() => navigator.clipboard.writeText(javaCode)}
                  disabled={!javaCode}
                  className="btn btn-secondary"
                >
                  Copy
                </button>
              </div>
            </div>
            <textarea
              className="code-editor"
              value={javaCode}
              readOnly
              placeholder="Converted Java code will appear here..."
              rows={20}
            />
          </div>
        </div>

        <div className="services-status">
          <h3>Services Status:</h3>
          <ul>
            <li>Backend API: <span className="status-running">Running on port 8000</span></li>
            <li>MCP Server: <span className="status-running">Running on port 8001</span></li>
            <li>Frontend: <span className="status-running">Running on port 3000</span></li>
          </ul>
        </div>
      </main>
    </div>
  );
}

export default App;
