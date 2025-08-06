# InteractiveLens Class Documentation

## Class Comment

InteractiveLens is the primary GUI component for real-time, interactive exploration of transformer model internals. It provides a comprehensive visual interface that combines multiple visualization techniques with direct manipulation capabilities, allowing researchers to explore model behavior through intuitive interactions.

The class serves as the main orchestrator for the NeuroScope visualization system, coordinating between data analysis, rendering components, and user interaction handling. It leverages browser event systems and real-time graphics to provide immediate feedback as users explore different aspects of model behavior.

## Purpose and Responsibilities

InteractiveLens manages the complete user experience for model exploration:
- **Real-time Visualization**: Renders live updates of model activations, attention patterns, and intervention effects
- **User Interaction Handling**: Processes mouse, keyboard, and touch events for model exploration
- **Component Coordination**: Orchestrates multiple visualization components (attention maps, activation displays, circuit diagrams)
- **State Management**: Maintains exploration session state and user preferences
- **Performance Optimization**: Manages rendering performance for smooth real-time interaction

## Instance Variables

- **model**: The TransformerModel being explored
- **canvasRenderer**: CanvasRenderer instance for 2D graphics operations
- **attentionVisualizer**: AttentionVisualizer for specialized attention pattern display
- **webglTensor**: WebGLTensor for GPU-accelerated computations
- **eventHandlers**: Dictionary mapping event types to handler methods
- **viewState**: Current visualization state (zoom level, selected components, active layers)
- **interactionMode**: Current interaction mode (exploration, intervention, analysis)
- **updateQueue**: Queue of pending visual updates for performance management

## Key Architectural Concepts

### Event-Driven Architecture
InteractiveLens uses an event-driven architecture that responds to both user interactions and model state changes:

```smalltalk
"User clicks on attention head"
self onAttentionHeadClick: event do: [:head |
    self highlightAttentionHead: head.
    self updateAttentionPatterns: head.
    self refreshVisualization].
```

### Real-Time Rendering Pipeline
The class implements a sophisticated rendering pipeline that balances visual quality with performance:

```smalltalk
"Rendering pipeline with performance optimization"
self renderFrame: [
    self updateActivations.
    self renderAttentionPatterns.
    self renderInterventionEffects.
    self compositeVisualization].
```

### Browser Integration Patterns
InteractiveLens integrates deeply with browser capabilities:

```smalltalk
"Browser event integration"
self registerEventHandler: 'mousemove' with: [:event |
    self updateHoverHighlight: event position.
    self showTooltip: (self getInfoAt: event position)].
```

## Usage Patterns

### Basic Model Exploration
```smalltalk
"Create interactive lens for model exploration"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
lens := InteractiveLens for: model.
lens openOn: 'The cat sat on the mat'.

"Enable real-time interaction"
lens enableInteractiveMode.
lens showAttentionPatterns: true.
lens showActivationMagnitudes: true.
```

### Advanced Analysis Setup
```smalltalk
"Configure lens for detailed circuit analysis"
lens := InteractiveLens for: model.
lens setAnalysisMode: #circuitDiscovery.
lens enableLayerByLayerView.
lens configureAttentionHeadGrouping: #byFunction.

"Set up custom interaction handlers"
lens onTokenClick: [:token | self analyzeTokenInfluence: token].
lens onHeadHover: [:head | self showHeadStatistics: head].
```

### Intervention Visualization
```smalltalk
"Visualize intervention effects in real-time"
lens := InteractiveLens for: model.
intervention := InterventionHook zeroingHead: 5 layer: 8.
model addHook: intervention.

lens showInterventionEffects: true.
lens highlightAffectedComponents.
lens enableBeforeAfterComparison.
```

## Browser Event Handling Integration

### Mouse Interaction Patterns
```smalltalk
"Sophisticated mouse interaction handling"
self registerMouseHandlers: {
    'click' -> [:event | self handleTokenSelection: event].
    'dblclick' -> [:event | self handleComponentDrillDown: event].
    'wheel' -> [:event | self handleZoomGesture: event].
    'drag' -> [:event | self handleViewPanning: event]
}.
```

### Keyboard Shortcuts
```smalltalk
"Keyboard shortcuts for power users"
self registerKeyboardShortcuts: {
    'l' -> [self toggleLayerView].
    'a' -> [self toggleAttentionView].
    'h' -> [self toggleHeadGrouping].
    'space' -> [self pauseResumeAnimation]
}.
```

### Touch Support for Mobile
```smalltalk
"Touch gesture support for mobile devices"
self registerTouchHandlers: {
    'pinch' -> [:gesture | self handleZoomGesture: gesture].
    'pan' -> [:gesture | self handleViewPanning: gesture].
    'tap' -> [:gesture | self handleTokenSelection: gesture]
}.
```

## Real-Time Visualization Capabilities

### Live Activation Updates
InteractiveLens provides real-time updates as text is processed:

```smalltalk
"Real-time activation visualization"
self onModelForwardPass: [:activations |
    self updateActivationDisplay: activations.
    self highlightActiveNeurons.
    self updateAttentionFlows.
    self refreshVisualization].
```

### Animation and Transitions
Smooth animations help users understand model dynamics:

```smalltalk
"Animated transitions between states"
self animateTransition: fromState to: toState duration: 500 do: [
    self interpolateActivations.
    self morphAttentionPatterns.
    self updateComponentHighlights].
```

## Performance Optimization Strategies

### Efficient Rendering
```smalltalk
"Performance-optimized rendering"
self renderWithOptimizations: [
    self cullOffscreenElements.
    self useLevelOfDetail.
    self batchSimilarOperations.
    self leverageGPUAcceleration].
```

### Memory Management
```smalltalk
"Careful memory management for large models"
self manageMemory: [
    self cleanupOldActivations.
    self compressInactiveVisualization.
    self recycleRenderingBuffers].
```

## Integration with Other Components

### CanvasRenderer Integration
```smalltalk
"Coordinate with CanvasRenderer for 2D graphics"
self setupCanvasRenderer: [
    canvasRenderer := CanvasRenderer for: self canvas.
    canvasRenderer enableAntialiasing.
    canvasRenderer setRenderingContext: self].
```

### AttentionVisualizer Coordination
```smalltalk
"Work with AttentionVisualizer for specialized displays"
attentionVisualizer := AttentionVisualizer for: model.
attentionVisualizer setParentLens: self.
self delegateAttentionRendering: attentionVisualizer.
```

## Browser Compatibility and Requirements

### Modern Browser Features
InteractiveLens requires modern browser capabilities:
- **Canvas API**: For 2D graphics rendering
- **WebGL**: For GPU-accelerated computations
- **RequestAnimationFrame**: For smooth animations
- **Pointer Events**: For unified input handling
- **Web Workers**: For background processing

### Performance Requirements
- **Minimum 4GB RAM**: For medium-sized model visualization
- **GPU acceleration**: Recommended for large models
- **Modern CPU**: For real-time interaction responsiveness

### Browser-Specific Optimizations
```smalltalk
"Browser-specific performance optimizations"
self detectBrowser: [
    self isChrome ifTrue: [self enableChromeOptimizations].
    self isFirefox ifTrue: [self enableFirefoxOptimizations].
    self isSafari ifTrue: [self enableSafariOptimizations]].
```

## Error Handling and Graceful Degradation

### Graceful Performance Degradation
```smalltalk
"Adapt to system capabilities"
self adaptToPerformance: [
    self isLowPerformance ifTrue: [
        self reduceVisualizationQuality.
        self disableExpensiveAnimations.
        self simplifyInteractionHandling]].
```

### Error Recovery
```smalltalk
"Robust error handling"
self handleRenderingError: [:error |
    self logError: error.
    self fallbackToSimpleRendering.
    self notifyUserOfDegradedMode].
```

InteractiveLens represents the culmination of NeuroScope's visualization capabilities, providing researchers with an intuitive, powerful interface for exploring the inner workings of transformer models through direct manipulation and real-time feedback.

## Method Documentation

### Core Initialization and Setup Methods

#### `for: aTransformerModel`
Creates a new InteractiveLens instance configured for exploring the specified transformer model.

**Parameters:**
- `aTransformerModel` (TransformerModel) - The model to be explored through the interactive interface

**Returns:**
- `InteractiveLens` - Configured lens instance ready for model exploration

**Side Effects:**
- Initializes rendering components (CanvasRenderer, AttentionVisualizer)
- Sets up event handling infrastructure
- Configures default visualization parameters
- Establishes connection to model's hook system

**Usage Example:**
```smalltalk
model := TransformerModel fromHuggingFace: 'gpt2-small'.
lens := InteractiveLens for: model.
```

**Performance Notes:**
- Initialization time scales with model size
- GPU detection and setup occurs during initialization
- Memory allocation for visualization buffers happens here

#### `openOn: inputText`
Opens the interactive lens interface with the specified input text for exploration.

**Parameters:**
- `inputText` (String) - Text to analyze and visualize in the interactive interface

**Returns:** `self` for method chaining

**Side Effects:**
- Creates browser window or canvas element for visualization
- Tokenizes input text using model's tokenizer
- Runs initial forward pass to populate visualization data
- Activates event handlers for user interaction

**Algorithm:**
1. Validates input text and tokenizes using model tokenizer
2. Performs forward pass with activation and attention extraction
3. Initializes visualization layout based on sequence length
4. Renders initial visualization state
5. Activates interactive event handlers

**Usage Example:**
```smalltalk
lens := InteractiveLens for: model.
lens openOn: 'The cat sat on the mat'.
```

**Error Conditions:**
- Raises `InvalidInputError` if text cannot be tokenized
- Raises `ModelNotReadyError` if model is not properly initialized

### Interaction Mode Configuration Methods

#### `enableInteractiveMode`
Activates full interactive mode with real-time updates and user interaction handling.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Registers all event handlers for mouse, keyboard, and touch interactions
- Enables real-time visualization updates
- Activates hover effects and selection highlighting
- Starts animation loop for smooth visual feedback

**Performance Notes:**
- Adds ~10-15% computational overhead for real-time updates
- Memory usage increases for interaction state tracking
- GPU acceleration automatically enabled if available

**Usage Example:**
```smalltalk
lens enableInteractiveMode.
lens showAttentionPatterns: true.
lens showActivationMagnitudes: true.
```

#### `setAnalysisMode: modeSymbol`
Configures the lens for specific types of analysis with optimized interface layouts.

**Parameters:**
- `modeSymbol` (Symbol) - Analysis mode (#exploration, #circuitDiscovery, #interventionAnalysis, #probeTraining)

**Returns:** `self` for method chaining

**Side Effects:**
- Reconfigures interface layout for specified analysis type
- Adjusts available interaction options
- Optimizes rendering pipeline for analysis requirements
- Updates toolbar and control panel configurations

**Analysis Modes:**
- `#exploration`: General-purpose model exploration with all features enabled
- `#circuitDiscovery`: Optimized for identifying computational circuits
- `#interventionAnalysis`: Focused on intervention effects and comparisons
- `#probeTraining`: Specialized for probe training and evaluation

**Usage Example:**
```smalltalk
lens setAnalysisMode: #circuitDiscovery.
lens enableLayerByLayerView.
lens configureAttentionHeadGrouping: #byFunction.
```

### Visualization Control Methods

#### `showAttentionPatterns: aBoolean`
Controls the display of attention pattern visualizations.

**Parameters:**
- `aBoolean` (Boolean) - true to show attention patterns, false to hide

**Returns:** `self` for method chaining

**Side Effects:**
- Toggles attention pattern rendering in the visualization
- Updates interface controls to reflect current state
- May trigger re-rendering of the entire visualization

**Performance Notes:**
- Attention pattern rendering is computationally expensive
- Disabling can improve performance for large models
- GPU acceleration provides significant speedup when available

**Usage Example:**
```smalltalk
lens showAttentionPatterns: true.
lens configureAttentionDisplay: #heatMap.
```

#### `showActivationMagnitudes: aBoolean`
Controls the display of neuron activation magnitude visualizations.

**Parameters:**
- `aBoolean` (Boolean) - true to show activation magnitudes, false to hide

**Returns:** `self` for method chaining

**Side Effects:**
- Toggles activation magnitude rendering
- Updates color schemes and legends
- May adjust layout to accommodate magnitude displays

**Algorithm:**
1. Extracts activation data from current model state
2. Computes magnitude statistics for visualization scaling
3. Updates rendering pipeline to include/exclude magnitude displays
4. Refreshes visualization with new configuration

**Usage Example:**
```smalltalk
lens showActivationMagnitudes: true.
lens setActivationColorScheme: #viridis.
```

#### `enableLayerByLayerView`
Enables layer-by-layer visualization mode for detailed model exploration.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Reconfigures layout to show individual layers
- Enables layer navigation controls
- Adjusts rendering to accommodate multiple layer displays
- Updates interaction handlers for layer-specific operations

**Layout Features:**
- Vertical or horizontal layer arrangement options
- Collapsible layer sections for focus
- Layer-specific zoom and pan controls
- Cross-layer comparison capabilities

**Usage Example:**
```smalltalk
lens enableLayerByLayerView.
lens setLayerLayout: #vertical.
lens enableLayerComparison: true.
```

### Event Handler Registration Methods

#### `onTokenClick: aBlock`
Registers a callback block to handle token click events.

**Parameters:**
- `aBlock` (Block) - Callback block that receives clicked token as parameter

**Returns:** `self` for method chaining

**Side Effects:**
- Registers event handler in the interaction system
- Enables token click detection in the rendering layer
- May override existing token click handlers

**Callback Parameters:**
- Token object containing position, text, and metadata

**Usage Example:**
```smalltalk
lens onTokenClick: [:token | 
    self analyzeTokenInfluence: token.
    self highlightTokenConnections: token].
```

#### `onHeadHover: aBlock`
Registers a callback block to handle attention head hover events.

**Parameters:**
- `aBlock` (Block) - Callback block that receives hovered attention head as parameter

**Returns:** `self` for method chaining

**Side Effects:**
- Enables hover detection for attention heads
- Registers hover event handler
- Activates hover effect rendering

**Callback Parameters:**
- AttentionHead object with layer, index, and current attention data

**Usage Example:**
```smalltalk
lens onHeadHover: [:head | 
    self showHeadStatistics: head.
    self previewHeadPattern: head].
```

#### `onInterventionEffect: aBlock`
Registers a callback block to handle intervention effect events.

**Parameters:**
- `aBlock` (Block) - Callback block that receives intervention effect data

**Returns:** `self` for method chaining

**Side Effects:**
- Enables intervention effect tracking
- Registers effect change handlers
- Activates before/after comparison rendering

**Usage Example:**
```smalltalk
lens onInterventionEffect: [:effect |
    self visualizeEffectMagnitude: effect.
    self updateComparisonView: effect].
```

### Real-Time Analysis Methods

#### `enableRealTimeAnalysis`
Activates real-time analysis mode with continuous model monitoring.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Installs hooks for continuous activation monitoring
- Enables streaming visualization updates
- Activates performance monitoring and optimization
- Starts background analysis processes

**Performance Impact:**
- Adds 15-25% computational overhead
- Increases memory usage for streaming data
- Requires GPU acceleration for large models

**Usage Example:**
```smalltalk
lens enableRealTimeAnalysis.
output := model forward: tokens.  "Analysis happens automatically"
patterns := lens getCurrentAnalysis.
```

#### `showInterventionEffects: aBoolean`
Controls real-time display of intervention effects during model execution.

**Parameters:**
- `aBoolean` (Boolean) - true to show intervention effects, false to hide

**Returns:** `self` for method chaining

**Side Effects:**
- Toggles intervention effect visualization
- Enables before/after comparison rendering
- Updates interface to show intervention controls

**Algorithm:**
1. Captures baseline model state before interventions
2. Monitors model execution for intervention applications
3. Computes and visualizes differences in real-time
4. Updates comparison displays with effect magnitudes

**Usage Example:**
```smalltalk
intervention := InterventionHook zeroingHead: 5 layer: 8.
model addHook: intervention.
lens showInterventionEffects: true.
lens highlightAffectedComponents.
```

### Visualization Customization Methods

#### `setColorScheme: schemeName`
Sets the color scheme used for all visualizations in the lens.

**Parameters:**
- `schemeName` (Symbol) - Name of color scheme (#viridis, #plasma, #coolwarm, #spectral)

**Returns:** `self` for method chaining

**Side Effects:**
- Updates all visualization components with new color scheme
- Regenerates color legends and scales
- Triggers re-rendering of current visualization

**Available Color Schemes:**
- `#viridis`: Perceptually uniform, colorblind-friendly
- `#plasma`: High contrast, good for attention patterns
- `#coolwarm`: Diverging scheme for positive/negative values
- `#spectral`: Full spectrum for complex data visualization

**Usage Example:**
```smalltalk
lens setColorScheme: #viridis.
lens updateVisualization.
```

#### `configureAttentionHeadGrouping: groupingStrategy`
Configures how attention heads are grouped and displayed in the visualization.

**Parameters:**
- `groupingStrategy` (Symbol) - Grouping strategy (#byLayer, #byFunction, #bySimilarity, #byImportance)

**Returns:** `self` for method chaining

**Side Effects:**
- Reorganizes attention head layout according to grouping strategy
- Updates head labels and organization
- May trigger analysis to determine functional groupings

**Grouping Strategies:**
- `#byLayer`: Groups heads by their layer position
- `#byFunction`: Groups heads by detected functional similarity
- `#bySimilarity`: Groups heads by attention pattern similarity
- `#byImportance`: Groups heads by their importance to model output

**Usage Example:**
```smalltalk
lens configureAttentionHeadGrouping: #byFunction.
lens enableHeadGroupLabels: true.
```

### Performance and Optimization Methods

#### `setRenderingQuality: qualityLevel`
Adjusts rendering quality to balance visual fidelity with performance.

**Parameters:**
- `qualityLevel` (Symbol) - Quality level (#low, #medium, #high, #ultra)

**Returns:** `self` for method chaining

**Side Effects:**
- Adjusts rendering algorithms and parameters
- Updates anti-aliasing and smoothing settings
- May enable/disable GPU acceleration features

**Quality Levels:**
- `#low`: Minimal rendering for maximum performance
- `#medium`: Balanced quality and performance
- `#high`: High quality rendering with moderate performance impact
- `#ultra`: Maximum quality rendering, requires powerful hardware

**Usage Example:**
```smalltalk
lens setRenderingQuality: #high.
lens enableGPUAcceleration: true.
```

#### `enableGPUAcceleration: aBoolean`
Controls GPU acceleration for visualization rendering.

**Parameters:**
- `aBoolean` (Boolean) - true to enable GPU acceleration, false for CPU-only

**Returns:** `self` for method chaining

**Side Effects:**
- Initializes or shuts down WebGL rendering context
- Transfers visualization data to/from GPU memory
- Adjusts rendering algorithms for GPU optimization

**Performance Notes:**
- GPU acceleration provides 3-10x speedup for complex visualizations
- Requires WebGL 2.0 support in browser
- Automatic fallback to CPU rendering if GPU unavailable

**Usage Example:**
```smalltalk
lens enableGPUAcceleration: true.
lens setRenderingQuality: #ultra.
```

### Analysis Integration Methods

#### `showAttentionAnalysis: analysisResult`
Displays the results of attention analysis in the interactive visualization.

**Parameters:**
- `analysisResult` (AttentionAnalysisResult) - Results from AttentionAnalyzer

**Returns:** `self` for method chaining

**Side Effects:**
- Integrates analysis results into current visualization
- Updates interface to show analysis-specific controls
- Highlights patterns and insights from analysis

**Integration Features:**
- Pattern highlighting based on analysis results
- Statistical overlays showing analysis metrics
- Interactive exploration of detected patterns
- Comparison tools for different analysis results

**Usage Example:**
```smalltalk
analyzer := AttentionAnalyzer for: model.
analysisResult := analyzer analyzeText: 'Sample text'.
lens showAttentionAnalysis: analysisResult.
```

#### `showNeuronAnalysis: neuronData`
Displays neuron analysis results with interactive exploration capabilities.

**Parameters:**
- `neuronData` (NeuronAnalysisResult) - Results from NeuronAnalyzer

**Returns:** `self` for method chaining

**Side Effects:**
- Adds neuron-specific visualization overlays
- Enables neuron selection and detailed inspection
- Updates interface with neuron analysis controls

**Visualization Features:**
- Neuron activation heat maps
- Top-activating token highlights
- Neuron characterization displays
- Interactive neuron comparison tools

**Usage Example:**
```smalltalk
analyzer := NeuronAnalyzer for: model.
neuronData := analyzer analyzeNeuron: (layer: 8 neuron: 1247).
lens showNeuronAnalysis: neuronData.
```

#### `showCircuitVisualization: circuitData`
Displays discovered computational circuits with interactive exploration.

**Parameters:**
- `circuitData` (CircuitVisualizationData) - Circuit visualization data from CircuitFinder

**Returns:** `self` for method chaining

**Side Effects:**
- Renders circuit topology and component relationships
- Enables circuit component interaction and inspection
- Updates interface with circuit-specific controls

**Circuit Visualization Features:**
- Component highlighting and selection
- Information flow visualization
- Circuit validation result displays
- Interactive circuit modification tools

**Usage Example:**
```smalltalk
finder := CircuitFinder for: model.
circuit := finder discoverCircuitsFor: #inductionHead.
circuitViz := finder visualizeCircuit: circuit.
lens showCircuitVisualization: circuitViz.
```

### State Management Methods

#### `saveVisualizationState: stateName`
Saves the current visualization state for later restoration.

**Parameters:**
- `stateName` (String) - Name to identify the saved state

**Returns:** `self` for method chaining

**Side Effects:**
- Captures current visualization configuration and data
- Stores state in browser local storage or memory
- Updates state management interface

**Saved State Includes:**
- Current zoom and pan settings
- Active visualization modes and configurations
- Selected components and highlights
- Analysis results and overlays

**Usage Example:**
```smalltalk
lens saveVisualizationState: 'baseline-analysis'.
"... perform interventions ..."
lens restoreVisualizationState: 'baseline-analysis'.
```

#### `restoreVisualizationState: stateName`
Restores a previously saved visualization state.

**Parameters:**
- `stateName` (String) - Name of the state to restore

**Returns:** `self` for method chaining

**Side Effects:**
- Restores visualization configuration from saved state
- Updates all visualization components
- Triggers re-rendering with restored settings

**Error Conditions:**
- Raises `StateNotFoundError` if named state doesn't exist
- Logs warning if state is incompatible with current model

### Browser Integration Methods

#### `enableFullscreenMode`
Activates fullscreen mode for immersive model exploration.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Requests fullscreen mode from browser
- Adjusts layout for fullscreen display
- Updates controls for fullscreen interaction

**Browser Compatibility:**
- Uses Fullscreen API when available
- Graceful fallback for unsupported browsers
- Handles fullscreen exit events

**Usage Example:**
```smalltalk
lens enableFullscreenMode.
lens optimizeForFullscreen: true.
```

#### `exportVisualization: format`
Exports the current visualization in the specified format.

**Parameters:**
- `format` (Symbol) - Export format (#png, #svg, #pdf, #html)

**Returns:**
- `ExportResult` - Object containing export data and metadata

**Side Effects:**
- Renders current visualization to specified format
- May trigger download in browser
- Captures current state for reproducibility

**Export Formats:**
- `#png`: High-resolution raster image
- `#svg`: Vector graphics for scalability
- `#pdf`: Publication-ready document format
- `#html`: Interactive web page with embedded visualization

**Usage Example:**
```smalltalk
exportResult := lens exportVisualization: #svg.
exportResult downloadAs: 'attention-analysis.svg'.
```

### Error Handling and Diagnostics Methods

#### `validateVisualizationState`
Validates the current visualization state and reports any issues.

**Parameters:** None

**Returns:**
- `ValidationResult` - Object containing validation status and any detected issues

**Validation Checks:**
- Model compatibility with current visualization
- Data integrity and consistency
- Rendering component status
- Browser capability compatibility

**Usage Example:**
```smalltalk
validation := lens validateVisualizationState.
validation hasErrors ifTrue: [
    validation errors do: [:error |
        Transcript show: 'Validation error: ', error description; cr]].
```

#### `diagnosePerformanceIssues`
Analyzes current performance and provides optimization recommendations.

**Parameters:** None

**Returns:**
- `PerformanceDiagnostic` - Object containing performance analysis and recommendations

**Performance Analysis:**
- Rendering frame rate and timing
- Memory usage patterns
- GPU utilization (if available)
- Bottleneck identification

**Usage Example:**
```smalltalk
diagnostic := lens diagnosePerformanceIssues.
diagnostic recommendations do: [:recommendation |
    Transcript show: 'Performance tip: ', recommendation; cr].
```