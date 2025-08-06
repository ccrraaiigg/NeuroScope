# CanvasRenderer Class Documentation

## Class Comment

CanvasRenderer provides high-performance 2D graphics rendering for NeuroScope's visualization components using the HTML5 Canvas API. It serves as the primary rendering engine for attention patterns, activation visualizations, and interactive graphics elements, optimized for real-time performance and visual clarity.

The class abstracts Canvas API complexities while providing specialized rendering methods for mechanistic interpretability visualizations. It implements efficient rendering pipelines, manages graphics state, and provides performance optimizations specifically tailored for transformer model visualization needs.

## Purpose and Responsibilities

CanvasRenderer manages all 2D graphics operations for NeuroScope:
- **Attention Pattern Rendering**: Specialized visualization of multi-head attention patterns
- **Activation Visualization**: Heat maps and magnitude displays for neuron activations
- **Interactive Graphics**: Hover effects, selection highlights, and user interface elements
- **Performance Optimization**: Efficient rendering pipelines with batching and caching
- **Canvas State Management**: Proper handling of graphics context state and transformations

## Instance Variables

- **canvas**: HTML5 Canvas element for rendering operations
- **context**: 2D rendering context from the canvas
- **renderingQueue**: Queue of pending rendering operations for batching
- **graphicsState**: Stack of saved graphics states for nested transformations
- **colorPalette**: Predefined color schemes for different visualization types
- **fontCache**: Cached font metrics and text rendering optimizations
- **imageCache**: Cached rendered elements for performance optimization
- **viewTransform**: Current view transformation matrix (zoom, pan, rotation)
- **clipRegions**: Active clipping regions for efficient rendering

## Key Architectural Concepts

### Rendering Pipeline Architecture
CanvasRenderer implements a sophisticated rendering pipeline that maximizes performance:

```smalltalk
"Optimized rendering pipeline"
self renderFrame: [
    self beginFrame.
    self processRenderingQueue.
    self batchSimilarOperations.
    self applyOptimizations.
    self commitFrame].
```

### State Management System
Proper graphics state management ensures consistent rendering:

```smalltalk
"Graphics state management"
self withGraphicsState: [
    self setTransform: viewMatrix.
    self setClipRegion: visibleArea.
    self renderVisualizationElements.
    self restoreState].
```

### Performance Optimization Strategies
Multiple optimization techniques ensure smooth real-time rendering:

```smalltalk
"Performance optimization techniques"
self optimizeRendering: [
    self cullOffscreenElements.
    self batchDrawCalls.
    self cacheStaticElements.
    self useRequestAnimationFrame].
```

## Canvas API Integration Patterns

### Context Configuration
```smalltalk
"Optimal canvas context setup"
self configureContext: [
    context imageSmoothingEnabled: true.
    context imageSmoothingQuality: 'high'.
    context textBaseline: 'middle'.
    context textAlign: 'center'].
```

### Advanced Drawing Operations
```smalltalk
"Advanced Canvas API usage"
self drawComplexShape: shape with: [
    context beginPath.
    context moveTo: shape startPoint.
    shape segments do: [:segment |
        context bezierCurveTo: segment controlPoints].
    context closePath.
    context fill].
```

## Attention Pattern Visualization

### Multi-Head Attention Rendering
CanvasRenderer provides specialized methods for attention visualization:

```smalltalk
"Render multi-head attention patterns"
self renderAttentionPattern: attentionMatrix heads: headCount do: [
    self setupAttentionColorScheme.
    attentionMatrix withIndexDo: [:attention :headIndex |
        self renderAttentionHead: attention 
             at: (self headPosition: headIndex)
             withIntensity: attention magnitude]].
```

### Attention Flow Visualization
```smalltalk
"Visualize attention flows between tokens"
self renderAttentionFlows: flows from: sourceTokens to: targetTokens do: [
    flows do: [:flow |
        self drawAttentionArc: flow
             from: (self tokenPosition: flow source)
             to: (self tokenPosition: flow target)
             withWeight: flow strength]].
```

### Interactive Attention Exploration
```smalltalk
"Interactive attention pattern exploration"
self enableAttentionInteraction: [
    self onHeadHover: [:head | self highlightAttentionHead: head].
    self onTokenClick: [:token | self showTokenAttentionPattern: token].
    self onPatternSelect: [:pattern | self analyzeAttentionPattern: pattern]].
```

## Activation Visualization Techniques

### Heat Map Rendering
```smalltalk
"Render activation heat maps"
self renderActivationHeatMap: activations bounds: bounds do: [
    self setupHeatMapColorScale.
    activations withIndexDo: [:activation :index |
        self drawHeatMapCell: activation
             at: (self cellPosition: index)
             withColor: (self heatMapColor: activation magnitude)]].
```

### Magnitude Visualization
```smalltalk
"Visualize activation magnitudes"
self renderActivationMagnitudes: magnitudes layout: layout do: [
    magnitudes do: [:magnitude |
        self drawMagnitudeBar: magnitude
             height: (self scaleToHeight: magnitude)
             color: (self magnitudeColor: magnitude)]].
```

### Neuron Activity Patterns
```smalltalk
"Render individual neuron activity patterns"
self renderNeuronActivity: neuronData timeSteps: steps do: [
    self setupActivityVisualization.
    neuronData do: [:neuron |
        self drawActivityTrace: neuron activity
             over: steps
             withStyle: self neuronActivityStyle]].
```

## Performance Optimization Implementation

### Efficient Batching
```smalltalk
"Batch similar rendering operations"
self batchRenderingOperations: operations do: [
    self groupOperationsByType: operations.
    self sortByRenderingOrder.
    self executeInBatches: self optimalBatchSize].
```

### Caching Strategies
```smalltalk
"Implement intelligent caching"
self manageCaching: [
    self cacheStaticElements.
    self invalidateChangedRegions.
    self precomputeExpensiveOperations.
    self recycleRenderingBuffers].
```

### Level-of-Detail Rendering
```smalltalk
"Adaptive level-of-detail based on zoom"
self renderWithLOD: zoomLevel do: [
    zoomLevel > self detailThreshold
        ifTrue: [self renderHighDetail]
        ifFalse: [self renderSimplified]].
```

## Interactive Graphics Elements

### Hover Effects
```smalltalk
"Implement smooth hover effects"
self renderHoverEffects: hoveredElements do: [
    hoveredElements do: [:element |
        self drawHighlight: element
             withGlow: self hoverGlowEffect
             animated: true]].
```

### Selection Highlighting
```smallttml
"Render selection highlights"
self renderSelectionHighlights: selectedElements do: [
    selectedElements do: [:element |
        self drawSelectionBorder: element
             withStyle: self selectionStyle
             animated: self selectionAnimation]].
```

### Interactive Feedback
```smalltalk
"Provide visual feedback for interactions"
self renderInteractiveFeedback: interaction do: [
    self drawFeedbackOverlay: interaction type.
    self animateFeedbackTransition.
    self scheduleCleanup: self feedbackDuration].
```

## Canvas API Integration and Optimization

### Context State Management
```smalltalk
"Efficient context state management"
self manageContextState: [
    self saveContextState.
    self applyTransformations.
    self setRenderingProperties.
    self executeDrawingOperations.
    self restoreContextState].
```

### Memory Management
```smalltalk
"Canvas memory optimization"
self optimizeCanvasMemory: [
    self clearUnusedImageData.
    self compactRenderingBuffers.
    self recycleCanvasElements.
    self manageTextureMemory].
```

### Browser-Specific Optimizations
```smalltalk
"Browser-specific rendering optimizations"
self applyBrowserOptimizations: [
    self isChrome ifTrue: [self enableChromeAcceleration].
    self isFirefox ifTrue: [self optimizeForFirefox].
    self isSafari ifTrue: [self applySafariWorkarounds]].
```

## Advanced Rendering Techniques

### Anti-Aliasing and Smoothing
```smalltalk
"High-quality anti-aliasing"
self enableAntiAliasing: [
    context imageSmoothingEnabled: true.
    context imageSmoothingQuality: 'high'.
    self useSubpixelPositioning.
    self applyCustomAntiAliasing].
```

### Gradient and Pattern Rendering
```smalltalk
"Advanced gradient and pattern effects"
self createGradientEffects: [
    gradient := context createLinearGradient: startPoint to: endPoint.
    self addColorStops: gradient.
    context fillStyle: gradient.
    self renderWithGradient].
```

### Text Rendering Optimization
```smalltalk
"Optimized text rendering"
self renderText: text at: position with: [
    self cacheTextMetrics: text.
    self selectOptimalFont: text length.
    self renderWithSubpixelAccuracy.
    self applyTextAntiAliasing].
```

## Integration with WebGL and GPU Acceleration

### WebGL Fallback Integration
```smalltalk
"Seamless WebGL integration when available"
self integrateWithWebGL: [
    self hasWebGLSupport
        ifTrue: [self delegateToWebGL: complexOperations]
        ifFalse: [self renderWithCanvas: allOperations]].
```

### GPU-Accelerated Operations
```smalltalk
"Leverage GPU acceleration where possible"
self useGPUAcceleration: [
    self identifyGPUCandidates: operations.
    self transferToGPU: gpuOperations.
    self executeOnGPU.
    self retrieveResults].
```

## Error Handling and Graceful Degradation

### Rendering Error Recovery
```smalltalk
"Robust error handling for rendering operations"
self handleRenderingError: error do: [
    self logRenderingError: error.
    self fallbackToSimpleRendering.
    self notifyPerformanceDegradation.
    self attemptRecovery].
```

### Performance Degradation Handling
```smalltalk
"Graceful performance degradation"
self handlePerformanceDegradation: [
    self reduceRenderingQuality.
    self simplifyVisualEffects.
    self increaseRenderingInterval.
    self disableExpensiveFeatures].
```

## Browser Compatibility and Requirements

### Canvas API Requirements
CanvasRenderer requires modern Canvas API features:
- **Canvas 2D Context**: Core rendering functionality
- **ImageData Support**: For pixel-level operations
- **Path2D Objects**: For complex shape rendering
- **Text Metrics**: For accurate text layout

### Performance Requirements
- **Hardware Acceleration**: Recommended for smooth rendering
- **Sufficient Memory**: For large visualization datasets
- **Modern Browser**: For optimal Canvas API support

### Feature Detection and Fallbacks
```smalltalk
"Feature detection and graceful fallbacks"
self detectCanvasFeatures: [
    self hasPath2DSupport ifFalse: [self usePathFallback].
    self hasImageSmoothingSupport ifFalse: [self useManualSmoothing].
    self hasTextMetricsSupport ifFalse: [self useTextFallback]].
```

CanvasRenderer serves as the high-performance foundation for NeuroScope's 2D visualization capabilities, providing researchers with smooth, responsive graphics that make complex transformer model internals accessible through intuitive visual representations.

## Method Documentation

### Core Initialization and Setup Methods

#### `for: aCanvasElement`
Creates a new CanvasRenderer instance for the specified HTML5 Canvas element.

**Parameters:**
- `aCanvasElement` (HTMLCanvasElement) - The canvas element to use for rendering operations

**Returns:**
- `CanvasRenderer` - Configured renderer instance ready for graphics operations

**Side Effects:**
- Obtains 2D rendering context from canvas element
- Initializes rendering state and optimization parameters
- Sets up default graphics properties and transformations
- Configures performance monitoring

**Usage Example:**
```smalltalk
canvas := document getElementById: 'visualization-canvas'.
renderer := CanvasRenderer for: canvas.
```

**Error Conditions:**
- Raises `InvalidCanvasError` if element is not a valid canvas
- Raises `ContextUnavailableError` if 2D context cannot be obtained

#### `configureForHighDPI: scaleFactor`
Configures the renderer for high-DPI displays with appropriate scaling.

**Parameters:**
- `scaleFactor` (Float) - DPI scale factor (typically 1.0, 1.5, 2.0, or 3.0)

**Returns:** `self` for method chaining

**Side Effects:**
- Adjusts canvas pixel dimensions for high-DPI rendering
- Updates coordinate transformations for proper scaling
- Reconfigures text rendering for crisp display
- Adjusts line widths and stroke parameters

**Algorithm:**
1. Detects device pixel ratio from browser
2. Scales canvas backing store to match device pixels
3. Adjusts CSS dimensions to maintain visual size
4. Updates all rendering parameters for new scale

**Usage Example:**
```smalltalk
renderer := CanvasRenderer for: canvas.
renderer configureForHighDPI: window devicePixelRatio.
```

### Core Rendering Methods

#### `beginFrame`
Initiates a new rendering frame with proper setup and optimization.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Clears canvas to background color
- Resets graphics state to defaults
- Initializes performance timing
- Prepares rendering queue for new frame

**Performance Notes:**
- Should be called once per frame before any drawing operations
- Automatically handles double buffering when available
- Optimizes clearing operations based on previous frame content

**Usage Example:**
```smalltalk
renderer beginFrame.
renderer renderVisualizationElements.
renderer commitFrame.
```

#### `commitFrame`
Completes the current rendering frame and presents results to display.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Flushes all pending rendering operations
- Presents completed frame to display
- Updates performance metrics
- Prepares for next frame

**Performance Notes:**
- Automatically synchronizes with browser's refresh rate
- Handles frame rate limiting for performance
- Provides timing information for optimization

#### `clearCanvas: backgroundColor`
Clears the entire canvas to the specified background color.

**Parameters:**
- `backgroundColor` (Color or String) - Background color for clearing operation

**Returns:** `self` for method chaining

**Side Effects:**
- Fills entire canvas with specified color
- Resets any clipping regions
- Invalidates cached rendering elements

**Usage Example:**
```smalltalk
renderer clearCanvas: Color white.
renderer clearCanvas: '#f0f0f0'.
```

### Attention Pattern Rendering Methods

#### `renderAttentionMatrix: attentionWeights at: position withColorScheme: colorScheme scale: scaleFactor`
Renders an attention weight matrix as a heat map visualization.

**Parameters:**
- `attentionWeights` (Matrix) - 2D matrix of attention weights (source × target tokens)
- `position` (Point) - Top-left position for rendering the matrix
- `colorScheme` (ColorScheme) - Color mapping for attention weight visualization
- `scaleFactor` (Float) - Scale factor for matrix size

**Returns:** `self` for method chaining

**Algorithm:**
1. Normalizes attention weights to [0,1] range for color mapping
2. Computes cell dimensions based on scale factor and matrix size
3. Iterates through matrix cells, mapping weights to colors
4. Renders each cell as filled rectangle with appropriate color
5. Adds optional grid lines and labels for clarity

**Computational Complexity:** O(N × M) where N=source tokens, M=target tokens

**Usage Example:**
```smalltalk
attentionMatrix := model getAttentionWeights: 5 head: 3.
renderer renderAttentionMatrix: attentionMatrix
    at: 100@100
    withColorScheme: ColorScheme viridis
    scale: 2.0.
```

**Performance Notes:**
- Large matrices (>512×512) may require level-of-detail rendering
- GPU acceleration available for matrices >128×128
- Automatic caching for repeated identical matrices

#### `renderAttentionFlow: flowData from: sourcePositions to: targetPositions withStyle: flowStyle`
Renders attention as flowing connections between token positions.

**Parameters:**
- `flowData` (Array of AttentionFlow) - Flow strength and direction data
- `sourcePositions` (Array of Point) - Screen positions of source tokens
- `targetPositions` (Array of Point) - Screen positions of target tokens
- `flowStyle` (FlowStyle) - Visual style for flow rendering (thickness, color, animation)

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes optimal flow paths using bezier curves or straight lines
2. Maps flow strengths to visual properties (thickness, opacity, color)
3. Renders flows in back-to-front order for proper layering
4. Applies animation effects if specified in flow style
5. Adds arrowheads or other directional indicators

**Usage Example:**
```smalltalk
flows := attentionAnalyzer computeAttentionFlows: attentionData.
sourcePos := self computeTokenPositions: sourceTokens.
targetPos := self computeTokenPositions: targetTokens.
renderer renderAttentionFlow: flows
    from: sourcePos
    to: targetPos
    withStyle: FlowStyle animated.
```

#### `renderMultiHeadAttention: headAttentions layout: layoutStrategy`
Renders multiple attention heads simultaneously with specified layout.

**Parameters:**
- `headAttentions` (Array of Matrix) - Attention matrices for each head
- `layoutStrategy` (Symbol) - Layout arrangement (#grid, #circular, #hierarchical)

**Returns:** `self` for method chaining

**Side Effects:**
- Computes optimal layout for specified number of heads
- Renders each head with appropriate scaling and positioning
- Adds head labels and identification
- Provides visual separation between heads

**Layout Strategies:**
- `#grid`: Rectangular grid arrangement
- `#circular`: Circular arrangement around center point
- `#hierarchical`: Tree-like hierarchical arrangement

**Usage Example:**
```smalltalk
headAttentions := model getAllAttentionHeads: layerIndex.
renderer renderMultiHeadAttention: headAttentions
    layout: #grid.
```

### Activation Visualization Methods

#### `renderActivationHeatMap: activations bounds: renderBounds colorScale: colorScale`
Renders neuron activations as a color-coded heat map.

**Parameters:**
- `activations` (Array or Matrix) - Activation values for visualization
- `renderBounds` (Rectangle) - Screen area for rendering the heat map
- `colorScale` (ColorScale) - Color mapping for activation magnitudes

**Returns:** `self` for method chaining

**Algorithm:**
1. Normalizes activation values for color mapping
2. Computes cell layout within specified bounds
3. Maps each activation to appropriate color
4. Renders cells with smooth color interpolation
5. Adds optional contour lines for value ranges

**Usage Example:**
```smalltalk
activations := model getLayerActivations: 8.
bounds := Rectangle origin: 50@50 extent: 400@300.
renderer renderActivationHeatMap: activations
    bounds: bounds
    colorScale: ColorScale plasma.
```

#### `renderActivationMagnitudes: magnitudes positions: positions style: barStyle`
Renders activation magnitudes as bar charts or other magnitude indicators.

**Parameters:**
- `magnitudes` (Array of Float) - Activation magnitude values
- `positions` (Array of Point) - Screen positions for each magnitude indicator
- `barStyle` (BarStyle) - Visual style for magnitude rendering

**Returns:** `self` for method chaining

**Algorithm:**
1. Normalizes magnitudes to consistent scale
2. Computes bar heights or indicator sizes
3. Renders magnitude indicators at specified positions
4. Applies color coding based on magnitude ranges
5. Adds optional value labels and legends

**Usage Example:**
```smalltalk
magnitudes := neuronAnalyzer computeActivationMagnitudes: neuronData.
positions := self computeNeuronPositions: neuronIndices.
renderer renderActivationMagnitudes: magnitudes
    positions: positions
    style: BarStyle vertical.
```

### Interactive Graphics Methods

#### `renderHoverHighlight: element withStyle: highlightStyle`
Renders hover effects for interactive elements.

**Parameters:**
- `element` (VisualizationElement) - Element to highlight
- `highlightStyle` (HighlightStyle) - Visual style for hover effect

**Returns:** `self` for method chaining

**Side Effects:**
- Renders highlight overlay on specified element
- May animate highlight appearance
- Updates cursor appearance for interactive feedback

**Highlight Effects:**
- Glow effects using shadow rendering
- Border highlighting with custom colors
- Scale animations for emphasis
- Opacity changes for subtle feedback

**Usage Example:**
```smalltalk
renderer renderHoverHighlight: hoveredToken
    withStyle: HighlightStyle glow.
```

#### `renderSelectionBorder: selectedElements borderStyle: borderStyle`
Renders selection borders around selected visualization elements.

**Parameters:**
- `selectedElements` (Array of VisualizationElement) - Elements to show as selected
- `borderStyle` (BorderStyle) - Visual style for selection borders

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes bounding boxes for selected elements
2. Renders selection borders with specified style
3. Handles overlapping selections appropriately
4. Adds selection handles or indicators if specified

**Usage Example:**
```smalltalk
renderer renderSelectionBorder: selectedHeads
    borderStyle: BorderStyle dashed.
```

### Performance Optimization Methods

#### `enableBatching: batchSize`
Enables batched rendering operations for improved performance.

**Parameters:**
- `batchSize` (Integer) - Number of operations to batch together

**Returns:** `self` for method chaining

**Side Effects:**
- Configures rendering queue for batched operations
- Adjusts memory allocation for batch processing
- May delay rendering until batch is full

**Performance Benefits:**
- Reduces Canvas API call overhead
- Improves GPU utilization
- Enables better optimization opportunities

**Usage Example:**
```smalltalk
renderer enableBatching: 64.
renderer renderManyElements: largeElementArray.
```

#### `enableLevelOfDetail: enabled`
Enables level-of-detail rendering for large visualizations.

**Parameters:**
- `enabled` (Boolean) - true to enable LOD rendering, false to disable

**Returns:** `self` for method chaining

**Side Effects:**
- Configures rendering algorithms for LOD support
- Sets up distance-based detail reduction
- May pre-compute simplified representations

**LOD Strategies:**
- Geometric simplification for distant elements
- Texture resolution reduction
- Element culling based on visibility
- Adaptive sampling for large datasets

**Usage Example:**
```smalltalk
renderer enableLevelOfDetail: true.
renderer setLODThresholds: #(0.5 1.0 2.0).
```

### Text and Label Rendering Methods

#### `renderText: text at: position withFont: fontSpec color: textColor`
Renders text with specified font and color at the given position.

**Parameters:**
- `text` (String) - Text content to render
- `position` (Point) - Screen position for text rendering
- `fontSpec` (FontSpecification) - Font family, size, and style
- `textColor` (Color) - Color for text rendering

**Returns:** `self` for method chaining

**Algorithm:**
1. Configures canvas context with font specifications
2. Measures text dimensions for proper positioning
3. Renders text with anti-aliasing and subpixel positioning
4. Applies any text effects (shadows, outlines)

**Usage Example:**
```smalltalk
renderer renderText: 'Layer 5, Head 3'
    at: 100@50
    withFont: (FontSpec family: 'Arial' size: 12)
    color: Color black.
```

#### `renderTokenLabels: tokens positions: positions labelStyle: labelStyle`
Renders labels for tokens with consistent styling and positioning.

**Parameters:**
- `tokens` (Array of String) - Token text content
- `positions` (Array of Point) - Screen positions for each token label
- `labelStyle` (LabelStyle) - Consistent styling for all labels

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes optimal label positioning to avoid overlaps
2. Renders background boxes or highlights if specified
3. Renders text content with consistent formatting
4. Handles text truncation for long tokens

**Usage Example:**
```smalltalk
tokens := model tokenizer decode: tokenIndices.
positions := self computeTokenPositions: tokens.
renderer renderTokenLabels: tokens
    positions: positions
    labelStyle: LabelStyle compact.
```

### Advanced Graphics Methods

#### `renderGradient: gradientSpec bounds: renderBounds`
Renders gradient fills within specified bounds.

**Parameters:**
- `gradientSpec` (GradientSpecification) - Gradient definition with colors and stops
- `renderBounds` (Rectangle) - Area to fill with gradient

**Returns:** `self` for method chaining

**Algorithm:**
1. Creates Canvas gradient object from specification
2. Configures gradient stops and color transitions
3. Applies gradient as fill or stroke style
4. Renders within specified bounds

**Gradient Types:**
- Linear gradients with directional control
- Radial gradients with center and radius
- Conic gradients for circular visualizations

**Usage Example:**
```smalltalk
gradient := GradientSpec linear
    from: Color blue to: Color red
    direction: #horizontal.
renderer renderGradient: gradient
    bounds: (Rectangle origin: 0@0 extent: 200@100).
```

#### `renderPath: pathData withStyle: pathStyle`
Renders complex paths with specified styling.

**Parameters:**
- `pathData` (PathData) - Path definition with curves, lines, and control points
- `pathStyle` (PathStyle) - Stroke and fill styling for path rendering

**Returns:** `self` for method chaining

**Algorithm:**
1. Constructs Canvas path from path data
2. Applies stroke and fill styles
3. Renders path with anti-aliasing
4. Handles path effects (shadows, gradients)

**Usage Example:**
```smalltalk
circuitPath := self computeCircuitPath: circuitComponents.
renderer renderPath: circuitPath
    withStyle: PathStyle circuitFlow.
```

### State Management Methods

#### `saveGraphicsState`
Saves the current graphics state for later restoration.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Pushes current transformation matrix onto state stack
- Saves current fill and stroke styles
- Preserves clipping regions and other context properties

**Usage Example:**
```smalltalk
renderer saveGraphicsState.
renderer setTransform: zoomedMatrix.
renderer renderDetailedView.
renderer restoreGraphicsState.
```

#### `restoreGraphicsState`
Restores the most recently saved graphics state.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Pops transformation matrix from state stack
- Restores previous fill and stroke styles
- Restores previous clipping regions

#### `setTransform: transformMatrix`
Sets the current transformation matrix for coordinate system changes.

**Parameters:**
- `transformMatrix` (TransformMatrix) - 2D transformation matrix for scaling, rotation, translation

**Returns:** `self` for method chaining

**Side Effects:**
- Updates canvas transformation matrix
- Affects all subsequent rendering operations
- May trigger coordinate system recalculation

**Usage Example:**
```smalltalk
zoomMatrix := TransformMatrix scale: 2.0 translate: -100@-100.
renderer setTransform: zoomMatrix.
```

### Error Handling and Diagnostics Methods

#### `validateRenderingContext`
Validates the current rendering context and reports any issues.

**Parameters:** None

**Returns:**
- `ValidationResult` - Object containing validation status and detected issues

**Validation Checks:**
- Canvas context availability and state
- Graphics state stack consistency
- Memory usage and resource limits
- Browser compatibility issues

**Usage Example:**
```smalltalk
validation := renderer validateRenderingContext.
validation hasErrors ifTrue: [
    self handleRenderingErrors: validation errors].
```

#### `measureRenderingPerformance: renderingBlock`
Measures performance of rendering operations for optimization.

**Parameters:**
- `renderingBlock` (Block) - Block containing rendering operations to measure

**Returns:**
- `PerformanceMeasurement` - Object containing timing and performance data

**Measured Metrics:**
- Frame rendering time
- Canvas API call frequency
- Memory allocation patterns
- GPU utilization (when available)

**Usage Example:**
```smalltalk
performance := renderer measureRenderingPerformance: [
    renderer renderComplexVisualization: visualizationData].
Transcript show: 'Rendering time: ', performance frameTime asString, 'ms'; cr.
```