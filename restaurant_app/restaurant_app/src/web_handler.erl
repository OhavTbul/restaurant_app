-module(web_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, get_menu_html(), Req0),
    {ok, Req, State}.

get_menu_html() ->
    <<"
<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>Restaurant Simulation Game</title>
    <style>
        body {
            font-family: 'Arial', sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            margin: 0;
            padding: 20px;
            min-height: 100vh;
            display: flex;
            justify-content: center;
            align-items: center;
        }
        
        .container {
            background: white;
            border-radius: 15px;
            padding: 40px;
            box-shadow: 0 20px 40px rgba(0,0,0,0.1);
            text-align: center;
            max-width: 800px;
            width: 100%;
        }
        
        h1 {
            color: #333;
            margin-bottom: 30px;
            font-size: 2.5em;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
        }
        
        .menu-item {
            background: linear-gradient(45deg, #4CAF50, #45a049);
            color: white;
            border: none;
            padding: 20px 40px;
            margin: 20px;
            border-radius: 25px;
            font-size: 1.5em;
            cursor: pointer;
            transition: all 0.3s ease;
            box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        
        .menu-item:hover {
            transform: translateY(-3px);
            box-shadow: 0 8px 25px rgba(0,0,0,0.3);
        }
        
        .menu-item:active {
            transform: translateY(0);
        }
        
        .difficulty-selector {
            margin: 20px 0;
            padding: 20px;
            background: #f8f9fa;
            border-radius: 10px;
        }
        
        .difficulty-selector label {
            font-weight: bold;
            color: #333;
            margin-right: 10px;
            font-size: 1.2em;
        }
        
        .difficulty-selector select {
            padding: 10px 20px;
            border: 2px solid #ddd;
            border-radius: 5px;
            font-size: 1.1em;
        }
        
        .status-display {
            margin-top: 20px;
            padding: 15px;
            background: #e9ecef;
            border-radius: 10px;
            min-height: 80px;
            text-align: left;
        }
        
        .status-display h3 {
            margin-top: 0;
            color: #333;
        }
        
        .restaurant-preview {
            margin-top: 30px;
            padding: 20px;
            background: #f0f8ff;
            border-radius: 10px;
            border: 2px solid #ddd;
        }
        
        .preview-canvas {
            border: 3px solid #333;
            border-radius: 8px;
            background: #fff;
            width: 100%;
            max-width: 700px;
            height: 400px;
        }
        
        .game-controls {
            margin: 30px 0;
        }
    </style>
</head>
<body>
    <div class='container'>
        <h1>🍽️ Restaurant Simulation Game</h1>
        
        <div class='difficulty-selector'>
            <label for='difficulty'>Difficulty:</label>
            <select id='difficulty'>
                <option value='easy'>Easy</option>
                <option value='normal' selected>Normal</option>
                <option value='hard'>Hard</option>
            </select>
        </div>
        
        <div class='game-controls'>
            <button class='menu-item' onclick='startGame()'>🚀 Start Restaurant Simulation</button>
        </div>
        
        <div class='status-display'>
            <h3>📊 Game Status</h3>
            <div id='status-text'>Ready to start the restaurant simulation!</div>
        </div>
        
        <div class='restaurant-preview'>
            <h3>🏪 Restaurant Preview</h3>
            <canvas id='previewCanvas' class='preview-canvas' width='700' height='400'></canvas>
        </div>
    </div>
    
    <script>
        let ws = null;
        let gameState = 'menu';
        
        // Initialize WebSocket connection
        function initWebSocket() {
            ws = new WebSocket('ws://localhost:8080/websocket');
            
            ws.onopen = function() {
                console.log('WebSocket connected');
                updateStatus('Connected to game server');
            };
            
            ws.onmessage = function(event) {
                const data = JSON.parse(event.data);
                handleGameUpdate(data);
            };
            
            ws.onclose = function() {
                console.log('WebSocket disconnected');
                updateStatus('Disconnected from game server');
            };
        }
        
        // Handle game updates from server
        function handleGameUpdate(data) {
            switch(data.type) {
                case 'status':
                    updateStatus(data.message);
                    break;
                case 'game_state':
                    gameState = data.state;
                    updateGameState(data.state);
                    break;
                case 'restaurant_update':
                    updateRestaurantPreview(data);
                    break;
            }
        }
        
        // Game control functions
        function startGame() {
            const difficulty = document.getElementById('difficulty').value;
            sendCommand({action: 'start', difficulty: difficulty});
            updateStatus('Starting restaurant simulation...');
        }
        
        // Send commands to server
        function sendCommand(command) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(command));
            }
        }
        
        // Update status display
        function updateStatus(message) {
            const statusDiv = document.getElementById('status-text');
            const timestamp = new Date().toLocaleTimeString();
            statusDiv.innerHTML += `<div>[${timestamp}] ${message}</div>`;
            statusDiv.scrollTop = statusDiv.scrollHeight;
        }
        
        // Update game state
        function updateGameState(state) {
            updateStatus(`Game state changed to: ${state}`);
        }
        
        // Update restaurant preview
        function updateRestaurantPreview(data) {
            const canvas = document.getElementById('previewCanvas');
            const ctx = canvas.getContext('2d');
            
            // Clear canvas
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            
            // Draw restaurant layout
            drawRestaurantLayout(ctx, data);
        }
        
        // Draw restaurant layout
        function drawRestaurantLayout(ctx, data) {
            // Clear canvas
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            
            // Draw background
            ctx.fillStyle = '#f8f9fa';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            
            // Draw title
            ctx.fillStyle = '#333';
            ctx.font = 'bold 20px Arial';
            ctx.fillText('Restaurant Layout', 280, 30);
            
            // Draw tables
            if (data.tables) {
                data.tables.forEach(([tableId, tableData]) => {
                    const x = tableData.position[0];
                    const y = tableData.position[1];
                    const occupied = tableData.occupied;
                    
                    // Table rectangle
                    ctx.fillStyle = occupied ? '#ff6b6b' : '#4CAF50';
                    ctx.fillRect(x, y, 60, 40);
                    ctx.strokeStyle = '#333';
                    ctx.lineWidth = 2;
                    ctx.strokeRect(x, y, 60, 40);
                    
                    // Table label
                    ctx.fillStyle = '#333';
                    ctx.font = 'bold 12px Arial';
                    ctx.fillText(tableId, x + 20, y + 25);
                    
                    // Customer indicator
                    if (occupied && tableData.customer) {
                        ctx.fillStyle = '#fff';
                        ctx.font = '10px Arial';
                        ctx.fillText('Customer', x + 15, y + 35);
                    }
                });
            }
            
            // Draw waiters
            if (data.waiters) {
                data.waiters.forEach(([waiterId, waiterData]) => {
                    const x = waiterData.position[0];
                    const y = waiterData.position[1];
                    const state = waiterData.state;
                    
                    // Waiter circle
                    ctx.fillStyle = state === 'idle' ? '#2196F3' : '#FF9800';
                    ctx.beginPath();
                    ctx.arc(x, y, 12, 0, 2 * Math.PI);
                    ctx.fill();
                    ctx.strokeStyle = '#1976D2';
                    ctx.lineWidth = 2;
                    ctx.stroke();
                    
                    // Waiter label
                    ctx.fillStyle = '#333';
                    ctx.font = 'bold 10px Arial';
                    ctx.fillText(waiterId, x + 20, y + 5);
                    
                    // State indicator
                    ctx.fillStyle = '#333';
                    ctx.font = '8px Arial';
                    ctx.fillText(state, x + 20, y + 15);
                });
            }
            
            // Draw machines
            if (data.machines) {
                data.machines.forEach(([machineId, machineData]) => {
                    const x = machineData.position[0];
                    const y = machineData.position[1];
                    const state = machineData.state;
                    
                    // Machine rectangle
                    ctx.fillStyle = state === 'cooking' ? '#FF9800' : '#9E9E9E';
                    ctx.fillRect(x, y, 50, 30);
                    ctx.strokeStyle = '#666';
                    ctx.lineWidth = 2;
                    ctx.strokeRect(x, y, 50, 30);
                    
                    // Machine label
                    ctx.fillStyle = '#333';
                    ctx.font = 'bold 10px Arial';
                    ctx.fillText(machineId, x + 10, y + 20);
                    
                    // State indicator
                    ctx.fillStyle = '#333';
                    ctx.font = '8px Arial';
                    ctx.fillText(state, x + 10, y + 30);
                });
            }
            
            // Draw customers
            if (data.customers) {
                data.customers.forEach(([customerId, customerData]) => {
                    const x = customerData.position[0];
                    const y = customerData.position[1];
                    const state = customerData.state;
                    
                    // Customer circle
                    ctx.fillStyle = '#E91E63';
                    ctx.beginPath();
                    ctx.arc(x, y, 8, 0, 2 * Math.PI);
                    ctx.fill();
                    ctx.strokeStyle = '#C2185B';
                    ctx.lineWidth = 1;
                    ctx.stroke();
                    
                    // Customer label
                    ctx.fillStyle = '#333';
                    ctx.font = '8px Arial';
                    ctx.fillText(customerId, x + 15, y + 5);
                    
                    // State indicator
                    ctx.fillStyle = '#333';
                    ctx.font = '6px Arial';
                    ctx.fillText(state, x + 15, y + 15);
                });
            }
            
            // Draw kitchen area
            ctx.fillStyle = '#FF9800';
            ctx.fillRect(50, 350, 600, 40);
            ctx.strokeStyle = '#F57C00';
            ctx.lineWidth = 3;
            ctx.strokeRect(50, 350, 600, 40);
            
            ctx.fillStyle = '#333';
            ctx.font = 'bold 16px Arial';
            ctx.fillText('Kitchen Area', 280, 375);
        }
        
        // Initialize when page loads
        window.onload = function() {
            initWebSocket();
            updateStatus('Restaurant simulation game loaded');
        };
    </script>
</body>
</html>
">>. 