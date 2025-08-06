# AttentionVisualizer Class Documentation

## Class Comment

AttentionVisualizer is a specialized rendering component dedicated to visualizing multi-head attention patterns in transformer models. It provides sophisticated algorithms for displaying attention weights, head interactions, and token-to-token relationships through intuitive visual representations optimized for mechanistic interpretability research.

The class implements advanced visualization techniques specifically designed for attention analysis, including attention flow diagrams, head comparison matrices, pattern clustering, and interactive exploration tools. It works closely with CanvasRenderer and InteractiveLens to provide seamless integration with the broader NeuroScope visualization ecosystem.

## Purpose and Responsibilities

AttentionVisualizer specializes in attention pattern visualization:
- **Multi-Head Attention Display**: Comprehensive visualization of all attention heads simultaneously
- **Attention Flow Rendering**: Visual representation of token-to-token attention relationships
- **Pattern Analysis Visualization**: Clustering and comparison of attention patterns across heads
- **Interactive Exploration**: User-driven exploration of attention mechanisms
- **Performance Optimization**: Efficient rendering of complex attention data structures

## Instance Variables

- **attentionData**: Current attention matrices for all layers and heads
- **canvasRenderer**: CanvasRenderer instance for graphics operations
- **parentLens**: Reference to the parent InteractiveLens component
- **visualizationMode**: Current visualization mode (matrix, flow, comparison, clustering)
- **headGroupings**: Logical groupings of attention heads by function or pattern
- **colorSchemes**: Color palettes for different visualization modes
- **interactionHandlers**: Event handlers for attention-specific interactions
- **renderingCache**: Cached rendered attention patterns for performance
- **animationState**: Current state of attention pattern animations

## Key Architectural Concepts

### Specialized Attention Rendering Pipeline
AttentionVisualizer implements a rendering pipeline optimized for attention data:

```smalltalk
"Attention-specific rendering pipeline"
self renderAttentionVisualization: [
    self preprocessAttentionData.
    self selectVisualizationAlgorithm.
    self computeLayoutParameters.
    self renderAttentionElements.
    self applyInteractiveOverlays].
```

### Multi-Scale Visualization
The class supports visualization at multiple scales and levels of detail:

```smalltalk
"Multi-scale attention visualization"
self renderAtScale: scale do: [
    scale = #overview ifTrue: [self renderAttentionOverview].
    scale = #detailed ifTrue: [self renderDetailedPatterns].
    scale = #individual ifTrue: [self renderSingleHeadAnalysis]].
```

### Pattern Recognition Integration
AttentionVisualizer integrates pattern recognition for intelligent visualization:

```smalltalk
"Pattern-aware visualization"
self visualizeWithPatternRecognition: [
    patterns := self detectAttentionPatterns.
    self groupSimilarPatterns: patterns.
    self renderPatternClusters.
    self highlightInterestingPatterns].
```

## Multi-Head Attention Display Techniques

### Matrix Visualization
```smalltalk
"Render attention matrices for multiple heads"
self renderAttentionMatrices: matrices layout: layout do: [
    matrices withIndexDo: [:matrix :headIndex |
        self renderAttentionMatrix: matrix
             at: (self headPosition: headIndex layout: layout)
             withColorScheme: (self colorSchemeForHead: headIndex)
             scale: self currentScale]].
```

### Head Comparison Display
```smalltalk
"Compare attention patterns across heads"
self renderHeadComparison: heads tokens: tokens do: [
    self setupComparisonLayout: heads size.
    heads withIndexDo: [:head :index |
        self renderHeadPattern: head
             position: (self comparisonPosition: index)
             highlighted: (self isHeadSelected: head)]].
```

### Attention Flow Diagrams
```smalltalk
"Visualize attention as flow between tokens"
self renderAttentionFlows: attentionWeights from: sourceTokens to: targetTokens do: [
    self computeFlowLayout: sourceTokens to: targetTokens.
    attentionWeights do: [:weight |
        self drawAttentionFlow: weight
             style: (self flowStyleFor: weight strength)
             animated: self flowAnimationEnabled]].
```

## Advanced Visualization Algorithms

### Pattern Clustering Visualization
```smalltalk
"Visualize clustered attention patterns"
self renderPatternClusters: clusters do: [
    clusters withIndexDo: [:cluster :index |
        self renderCluster: cluster
             at: (self clusterPosition: index)
             withRepresentative: cluster representative
             members: cluster members]].
```

### Attention Entropy Visualization
```smalltalk
"Visualize attention entropy and distribution"
self renderAttentionEntropy: entropyData do: [
    self setupEntropyColorScale.
    entropyData withIndexDo: [:entropy :position |
        self drawEntropyIndicator: entropy
             at: position
             withIntensity: (self entropyToIntensity: entropy)]].
```

### Temporal Attention Evolution
```smalltalk
"Visualize how attention patterns evolve over time"
self renderAttentionEvolution: timeSteps do: [
    self setupTemporalLayout: timeSteps size.
    timeSteps withIndexDo: [:step :time |
        self renderAttentionSnapshot: step
             at: (self temporalPosition: time)
             withTransition: (self transitionTo: time + 1)]].
```

## Interactive Exploration Features

### Head Selection and Highlighting
```smalltalk
"Interactive head selection and highlighting"
self enableHeadInteraction: [
    self onHeadClick: [:head |
        self selectHead: head.
        self highlightHeadPattern: head.
        self showHeadStatistics: head].
    
    self onHeadHover: [:head |
        self previewHeadPattern: head.
        self showHeadTooltip: head]].
```

### Token-Level Interaction
```smalltalk
"Token-level attention exploration"
self enableTokenInteraction: [
    self onTokenClick: [:token |
        self showTokenAttentionPattern: token.
        self highlightAttentionSources: token.
        self displayTokenInfluence: token].
    
    self onTokenHover: [:token |
        self previewTokenAttention: token.
        self showAttentionTooltip: token]].
```

### Pattern Comparison Tools
```smalltalk
"Interactive pattern comparison"
self enablePatternComparison: [
    self onPatternSelect: [:pattern |
        self addToComparison: pattern.
        self updateComparisonView.
        self highlightDifferences].
    
    self onComparisonMode: [
        self showSideBySideComparison.
        self enableDifferenceHighlighting.
        self provideSimilarityMetrics]].
```

## Visualization Mode Implementations

### Matrix Mode
```smalltalk
"Traditional attention matrix visualization"
self renderMatrixMode: attentionData do: [
    self setupMatrixLayout.
    self renderAttentionMatrix: attentionData
         withRowLabels: self sourceTokens
         columnLabels: self targetTokens
         colorScheme: self matrixColorScheme].
```

### Flow Mode
```smalltalk
"Attention flow visualization mode"
self renderFlowMode: attentionData do: [
    self computeFlowLayout: attentionData.
    self renderTokenNodes: self allTokens.
    self renderAttentionEdges: attentionData
         withThickness: #proportionalToWeight
         animation: self flowAnimation].
```

### Clustering Mode
```smalltalk
"Pattern clustering visualization mode"
self renderClusteringMode: patterns do: [
    clusters := self clusterPatterns: patterns.
    self renderClusterOverview: clusters.
    self enableClusterDrillDown.
    self showClusterStatistics: clusters].
```

## Browser Compatibility and Optimization

### Cross-Browser Rendering
```smalltalk
"Ensure consistent rendering across browsers"
self ensureBrowserCompatibility: [
    self detectBrowserCapabilities.
    self adaptRenderingTechniques.
    self applyBrowserSpecificOptimizations.
    self validateRenderingQuality].
```

### Performance Optimization for Large Models
```smalltalk
"Optimize for large attention matrices"
self optimizeForLargeModels: [
    self implementLevelOfDetail.
    self useProgressiveRendering.
    self enableDataStreaming.
    self optimizeMemoryUsage].
```

### Mobile and Touch Support
```smalltalk
"Mobile-optimized attention visualization"
self enableMobileSupport: [
    self adaptLayoutForMobile.
    self implementTouchGestures.
    self optimizeForSmallScreens.
    self reduceComputationalComplexity].
```

## Advanced Rendering Techniques

### Attention Weight Interpolation
```smalltalk
"Smooth interpolation between attention states"
self interpolateAttentionWeights: fromWeights to: toWeights progress: t do: [
    interpolatedWeights := self lerp: fromWeights to: toWeights by: t.
    self renderInterpolatedAttention: interpolatedWeights.
    self updateVisualizationSmooth].
```

### Multi-Layer Attention Visualization
```smalltalk
"Visualize attention across multiple layers"
self renderMultiLayerAttention: layerAttentions do: [
    self setupLayerLayout: layerAttentions size.
    layerAttentions withIndexDo: [:attention :layer |
        self renderLayerAttention: attention
             at: (self layerPosition: layer)
             withDepthCue: (self depthCueForLayer: layer)]].
```

### Attention Pattern Animation
```smalltalk
"Animate attention pattern changes"
self animateAttentionTransition: fromPattern to: toPattern do: [
    self setupTransitionAnimation.
    self interpolatePatternChanges: fromPattern to: toPattern.
    self renderAnimationFrames.
    self completeTransition].
```

## Integration with Analysis Tools

### Circuit Discovery Integration
```smalltalk
"Integrate with circuit discovery visualization"
self visualizeDiscoveredCircuits: circuits do: [
    circuits do: [:circuit |
        self highlightCircuitAttention: circuit.
        self showCircuitFlow: circuit.
        self renderCircuitComponents: circuit]].
```

### Probe Result Visualization
```smalltalk
"Visualize probe results in attention context"
self visualizeProbeResults: probeData with: attentionData do: [
    self overlayProbeResults: probeData on: attentionData.
    self highlightProbeRelevantAttention.
    self showProbeAttentionCorrelation].
```

### Intervention Effect Visualization
```smalltalk
"Show intervention effects on attention patterns"
self visualizeInterventionEffects: beforeAttention after: afterAttention do: [
    self computeAttentionDifference: beforeAttention after: afterAttention.
    self renderDifferenceVisualization.
    self highlightAffectedPatterns.
    self showInterventionImpact].
```

## Error Handling and Graceful Degradation

### Data Validation and Error Recovery
```smalltalk
"Robust handling of attention data issues"
self validateAttentionData: data do: [
    self checkDataIntegrity: data.
    self handleMissingData: data.
    self validateDimensions: data.
    self recoverFromErrors: data].
```

### Performance Degradation Strategies
```smalltalk
"Graceful performance degradation"
self handlePerformanceIssues: [
    self detectPerformanceProblems.
    self reduceVisualizationComplexity.
    self simplifyRenderingAlgorithms.
    self notifyUserOfDegradation].
```

## Customization and Extension Points

### Custom Visualization Modes
```smalltalk
"Support for custom visualization modes"
self registerCustomMode: modeName implementation: modeImplementation do: [
    self addModeToSelector: modeName.
    self registerModeRenderer: modeImplementation.
    self enableModeCustomization: modeName].
```

### Color Scheme Customization
```smalltalk
"Customizable color schemes for attention visualization"
self customizeColorScheme: schemeName with: [
    self defineColorMapping: attentionWeights to: colors.
    self setColorInterpolation: interpolationMethod.
    self enableColorBlindnessSupport].
```

AttentionVisualizer provides researchers with sophisticated, interactive tools for understanding the complex attention mechanisms that drive transformer model behavior, making abstract attention patterns concrete and explorable through advanced visualization techniques.

## Method Documentation

### Core Initialization and Configuration Methods

#### `for: aTransformerModel`
Creates a new AttentionVisualizer instance configured for the specified transformer model.

**Parameters:**
- `aTransformerModel` (TransformerModel) - The model whose attention patterns will be visualized

**Returns:**
- `AttentionVisualizer` - Configured visualizer instance ready for attention rendering

**Side Effects:**
- Analyzes model architecture to determine attention head configuration
- Initializes rendering components and color schemes
- Sets up default visualization parameters
- Configures pattern detection algorithms

**Usage Example:**
```smalltalk
model := TransformerModel fromHuggingFace: 'gpt2-small'.
visualizer := AttentionVisualizer for: model.
```

**Performance Notes:**
- Initialization time scales with model size and number of attention heads
- Memory allocation for attention data caching occurs during setup

#### `setParentLens: anInteractiveLens`
Establishes connection with parent InteractiveLens for coordinated visualization.

**Parameters:**
- `anInteractiveLens` (InteractiveLens) - Parent lens component for integration

**Returns:** `self` for method chaining

**Side Effects:**
- Registers with parent lens for event coordination
- Shares rendering resources and state management
- Enables coordinated updates and interactions

**Usage Example:**
```smalltalk
lens := InteractiveLens for: model.
visualizer := AttentionVisualizer for: model.
visualizer setParentLens: lens.
```

### Attention Matrix Visualization Methods

#### `renderAttentionMatrix: attentionWeights at: position withColorScheme: colorScheme scale: scaleFactor`
Renders a single attention matrix as a heat map visualization.

**Parameters:**
- `attentionWeights` (Matrix) - 2D attention weight matrix (query × key positions)
- `position` (Point) - Screen position for matrix rendering
- `colorScheme` (ColorScheme) - Color mapping for attention weight visualization
- `scaleFactor` (Float) - Scale factor for matrix display size

**Returns:** `self` for method chaining

**Algorithm:**
1. Normalizes attention weights to [0,1] range for consistent color mapping
2. Computes optimal cell dimensions based on scale and matrix size
3. Applies color scheme to map weights to visual colors
4. Renders matrix cells with smooth color interpolation
5. Adds optional grid lines, labels, and legends

**Computational Complexity:** O(N × M) where N=query length, M=key length

**Usage Example:**
```smalltalk
attentionMatrix := model getAttentionWeights: layer: 5 head: 3.
visualizer renderAttentionMatrix: attentionMatrix
    at: 100@100
    withColorScheme: ColorScheme viridis
    scale: 1.5.
```

**Performance Notes:**
- Large matrices (>1024×1024) automatically use level-of-detail rendering
- GPU acceleration provides 5-10x speedup for matrices >256×256
- Intelligent caching prevents redundant rendering of identical matrices

#### `renderMultiHeadComparison: headAttentions tokens: tokenArray layout: layoutStrategy`
Renders multiple attention heads side-by-side for comparative analysis.

**Parameters:**
- `headAttentions` (Array of Matrix) - Attention matrices for each head to compare
- `tokenArray` (Array of String) - Token labels for matrix axes
- `layoutStrategy` (Symbol) - Layout arrangement (#grid, #horizontal, #vertical, #circular)

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes optimal layout dimensions for specified number of heads
2. Calculates individual matrix sizes to fit within available space
3. Renders each attention matrix with consistent scaling and color schemes
4. Adds head labels, indices, and comparative highlighting
5. Provides visual separation and alignment guides

**Layout Strategies:**
- `#grid`: Rectangular grid arrangement for many heads
- `#horizontal`: Single row arrangement for few heads
- `#vertical`: Single column arrangement for detailed comparison
- `#circular`: Circular arrangement around central focus point

**Usage Example:**
```smalltalk
headAttentions := model getAttentionHeads: layer: 8.
tokens := model tokenizer decode: tokenIndices.
visualizer renderMultiHeadComparison: headAttentions
    tokens: tokens
    layout: #grid.
```

### Attention Flow Visualization Methods

#### `renderAttentionFlows: flowData from: sourceTokens to: targetTokens style: flowStyle`
Renders attention as flowing connections between token positions.

**Parameters:**
- `flowData` (Array of AttentionFlow) - Flow strength and direction information
- `sourceTokens` (Array of TokenPosition) - Source token positions and metadata
- `targetTokens` (Array of TokenPosition) - Target token positions and metadata
- `flowStyle` (FlowStyle) - Visual styling for flow rendering

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes optimal flow paths using bezier curves or direct lines
2. Maps flow strengths to visual properties (thickness, opacity, color intensity)
3. Sorts flows by strength for proper layering (weak flows behind strong ones)
4. Renders flows with smooth curves and optional animation effects
5. Adds directional indicators (arrowheads) and flow labels

**Flow Style Options:**
- Thickness proportional to attention weight
- Color intensity based on flow strength
- Animation speed reflecting attention dynamics
- Transparency for overlapping flow visualization

**Usage Example:**
```smalltalk
flows := attentionAnalyzer computeFlowData: attentionMatrix.
sourcePos := self computeTokenPositions: sourceTokens.
targetPos := self computeTokenPositions: targetTokens.
visualizer renderAttentionFlows: flows
    from: sourcePos
    to: targetPos
    style: FlowStyle animated.
```

#### `renderAttentionGraph: graphData layout: graphLayout`
Renders attention patterns as a graph with tokens as nodes and attention as edges.

**Parameters:**
- `graphData` (AttentionGraph) - Graph representation of attention relationships
- `graphLayout` (Symbol) - Graph layout algorithm (#forceDirected, #hierarchical, #circular)

**Returns:** `self` for method chaining

**Algorithm:**
1. Applies specified layout algorithm to position token nodes
2. Computes edge routing to minimize visual clutter
3. Renders nodes with size proportional to token importance
4. Renders edges with thickness proportional to attention weight
5. Applies interactive highlighting and selection capabilities

**Graph Layout Algorithms:**
- `#forceDirected`: Physics-based layout with natural clustering
- `#hierarchical`: Tree-like layout showing attention hierarchy
- `#circular`: Circular arrangement with attention flows as chords

**Usage Example:**
```smalltalk
graph := attentionAnalyzer buildAttentionGraph: attentionData.
visualizer renderAttentionGraph: graph
    layout: #forceDirected.
```

### Pattern Analysis Visualization Methods

#### `renderPatternClusters: clusterData highlightSimilarities: highlightEnabled`
Visualizes clustered attention patterns with similarity highlighting.

**Parameters:**
- `clusterData` (Array of AttentionCluster) - Clustered attention pattern data
- `highlightEnabled` (Boolean) - Whether to highlight pattern similarities

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes cluster layout to show relationships between pattern groups
2. Renders representative pattern for each cluster
3. Shows cluster membership and similarity metrics
4. Highlights similar patterns across clusters when enabled
5. Provides interactive cluster exploration capabilities

**Cluster Visualization Features:**
- Representative pattern display for each cluster
- Cluster size and cohesion indicators
- Inter-cluster similarity connections
- Interactive cluster expansion and exploration

**Usage Example:**
```smalltalk
clusters := attentionAnalyzer clusterAttentionPatterns: allHeadPatterns.
visualizer renderPatternClusters: clusters
    highlightSimilarities: true.
```

#### `renderAttentionEntropy: entropyData colorScale: colorScale`
Visualizes attention entropy to show concentration vs. diffusion patterns.

**Parameters:**
- `entropyData` (Array of Float) - Entropy values for each attention head or position
- `colorScale` (ColorScale) - Color mapping for entropy visualization

**Returns:** `self` for method chaining

**Algorithm:**
1. Normalizes entropy values to consistent scale
2. Maps entropy values to colors using specified color scale
3. Renders entropy visualization as heat map or bar chart
4. Adds entropy scale legend and interpretation guides
5. Highlights high and low entropy regions for analysis

**Entropy Interpretation:**
- Low entropy: Focused, concentrated attention
- High entropy: Diffuse, distributed attention
- Entropy patterns reveal attention head specialization

**Usage Example:**
```smalltalk
entropyData := attentionAnalyzer computeAttentionEntropy: attentionMatrices.
visualizer renderAttentionEntropy: entropyData
    colorScale: ColorScale coolwarm.
```

### Interactive Exploration Methods

#### `enableHeadSelection: selectionHandler`
Enables interactive selection of attention heads with custom handling.

**Parameters:**
- `selectionHandler` (Block) - Callback block executed when head is selected

**Returns:** `self` for method chaining

**Side Effects:**
- Registers click and hover event handlers for attention heads
- Enables visual feedback for head selection
- Activates head highlighting and focus capabilities

**Selection Handler Parameters:**
- Selected AttentionHead object with layer, index, and current data

**Usage Example:**
```smalltalk
visualizer enableHeadSelection: [:head |
    self showHeadDetails: head.
    self highlightHeadPattern: head.
    self updateAnalysisPanel: head].
```

#### `enableTokenInteraction: interactionHandler`
Enables interactive exploration of token-level attention patterns.

**Parameters:**
- `interactionHandler` (Block) - Callback block for token interaction events

**Returns:** `self` for method chaining

**Side Effects:**
- Registers event handlers for token clicks and hovers
- Enables token highlighting and attention pattern display
- Activates token-specific analysis capabilities

**Interaction Types:**
- Click: Select token and show detailed attention patterns
- Hover: Preview token attention without full selection
- Double-click: Focus on token and hide irrelevant connections

**Usage Example:**
```smalltalk
visualizer enableTokenInteraction: [:token :eventType |
    eventType = #click ifTrue: [self analyzeTokenAttention: token].
    eventType = #hover ifTrue: [self previewTokenPattern: token]].
```

### Animation and Transition Methods

#### `animateAttentionTransition: fromPattern to: toPattern duration: milliseconds`
Animates smooth transitions between different attention patterns.

**Parameters:**
- `fromPattern` (AttentionPattern) - Starting attention pattern
- `toPattern` (AttentionPattern) - Target attention pattern
- `milliseconds` (Integer) - Duration of transition animation

**Returns:** `self` for method chaining

**Algorithm:**
1. Computes interpolation path between attention patterns
2. Generates intermediate frames using smooth interpolation
3. Renders animation frames at consistent frame rate
4. Handles pattern alignment and correspondence matching
5. Provides completion callback for chained animations

**Animation Features:**
- Smooth weight interpolation between patterns
- Morphing of attention flow visualizations
- Coordinated color and opacity transitions
- Easing functions for natural motion

**Usage Example:**
```smalltalk
baselinePattern := model getAttentionPattern: baselineInput.
interventionPattern := model getAttentionPattern: interventionInput.
visualizer animateAttentionTransition: baselinePattern
    to: interventionPattern
    duration: 1000.
```

#### `enableFlowAnimation: animationSpeed`
Enables animated flow visualization for attention patterns.

**Parameters:**
- `animationSpeed` (Float) - Speed multiplier for flow animation (1.0 = normal speed)

**Returns:** `self` for method chaining

**Side Effects:**
- Starts continuous animation loop for attention flows
- Enables particle-based flow visualization
- Activates dynamic color and opacity effects

**Animation Effects:**
- Flowing particles along attention connections
- Pulsing intensity based on attention strength
- Directional flow indicators with smooth motion
- Synchronized animation across multiple heads

**Usage Example:**
```smalltalk
visualizer enableFlowAnimation: 1.5.
visualizer renderAttentionFlows: flowData
    from: sourceTokens
    to: targetTokens
    style: FlowStyle animated.
```

### Specialized Visualization Modes

#### `renderInductionHeadPattern: inductionData highlightPattern: patternType`
Specialized visualization for induction head attention patterns.

**Parameters:**
- `inductionData` (InductionHeadData) - Detected induction head pattern data
- `patternType` (Symbol) - Type of pattern highlighting (#lookback, #copy, #both)

**Returns:** `self` for method chaining

**Algorithm:**
1. Identifies induction head pattern components (lookback and copy phases)
2. Renders attention matrix with induction-specific highlighting
3. Shows pattern flow from lookback positions to copy targets
4. Highlights repeated token sequences and their relationships
5. Provides pattern strength indicators and confidence measures

**Pattern Highlighting:**
- `#lookback`: Highlights attention to previous token occurrences
- `#copy`: Highlights copying from previous positions
- `#both`: Shows complete induction head pattern cycle

**Usage Example:**
```smalltalk
inductionData := attentionAnalyzer detectInductionHeads: attentionMatrix.
visualizer renderInductionHeadPattern: inductionData
    highlightPattern: #both.
```

#### `renderSyntacticAttentionPattern: syntaxData linguisticFeatures: features`
Specialized visualization for syntactic attention patterns.

**Parameters:**
- `syntaxData` (SyntacticAttentionData) - Detected syntactic attention patterns
- `features` (Array of Symbol) - Linguistic features to highlight

**Returns:** `self` for method chaining

**Algorithm:**
1. Overlays syntactic relationship information on attention patterns
2. Highlights attention between syntactically related tokens
3. Shows grammatical role relationships and dependencies
4. Renders linguistic feature annotations and labels
5. Provides syntactic pattern strength and confidence indicators

**Linguistic Features:**
- Part-of-speech relationships
- Syntactic dependencies (subject-verb, modifier-noun)
- Phrase structure boundaries
- Grammatical role assignments

**Usage Example:**
```smalltalk
syntaxData := attentionAnalyzer detectSyntacticAttention: attentionMatrix.
visualizer renderSyntacticAttentionPattern: syntaxData
    linguisticFeatures: #(subjectVerb modifierNoun).
```

### Performance Optimization Methods

#### `enableGPUAcceleration: accelerationLevel`
Configures GPU acceleration for attention visualization rendering.

**Parameters:**
- `accelerationLevel` (Symbol) - Level of GPU acceleration (#basic, #advanced, #maximum)

**Returns:** `self` for method chaining

**Side Effects:**
- Initializes WebGL rendering context for GPU operations
- Transfers attention data to GPU memory
- Configures GPU-optimized rendering algorithms

**Acceleration Levels:**
- `#basic`: GPU-accelerated matrix rendering only
- `#advanced`: GPU acceleration for flows and animations
- `#maximum`: Full GPU pipeline with compute shaders

**Usage Example:**
```smalltalk
visualizer enableGPUAcceleration: #advanced.
visualizer renderLargeAttentionMatrix: hugeMatrix.
```

#### `configureLevelOfDetail: lodSettings`
Configures level-of-detail rendering for large attention visualizations.

**Parameters:**
- `lodSettings` (LevelOfDetailSettings) - Configuration for adaptive detail reduction

**Returns:** `self` for method chaining

**Side Effects:**
- Sets up distance-based detail reduction algorithms
- Configures simplified rendering for distant or small elements
- Enables automatic quality adjustment based on zoom level

**LOD Features:**
- Automatic matrix simplification for distant views
- Adaptive flow rendering based on zoom level
- Progressive detail loading for large datasets
- Quality-performance trade-off optimization

**Usage Example:**
```smalltalk
lodSettings := LevelOfDetailSettings new
    nearDistance: 1.0;
    farDistance: 10.0;
    qualityLevels: #(high medium low).
visualizer configureLevelOfDetail: lodSettings.
```

### Export and Sharing Methods

#### `exportVisualization: format options: exportOptions`
Exports current attention visualization in specified format.

**Parameters:**
- `format` (Symbol) - Export format (#png, #svg, #pdf, #interactive)
- `exportOptions` (ExportOptions) - Configuration for export process

**Returns:**
- `ExportResult` - Object containing exported data and metadata

**Algorithm:**
1. Captures current visualization state and configuration
2. Renders visualization to specified format with high quality
3. Includes metadata and reproduction information
4. Handles format-specific optimization and compression
5. Provides download or sharing capabilities

**Export Formats:**
- `#png`: High-resolution raster image for publications
- `#svg`: Vector graphics for scalable visualization
- `#pdf`: Publication-ready document with embedded visualization
- `#interactive`: Self-contained HTML with interactive features

**Usage Example:**
```smalltalk
exportOptions := ExportOptions new
    resolution: #high;
    includeMetadata: true;
    compression: #optimal.
result := visualizer exportVisualization: #svg
    options: exportOptions.
result downloadAs: 'attention-analysis.svg'.
```

### Error Handling and Diagnostics Methods

#### `validateAttentionData: attentionData`
Validates attention data for visualization compatibility and correctness.

**Parameters:**
- `attentionData` (AttentionData) - Attention data to validate

**Returns:**
- `ValidationResult` - Object containing validation status and any issues found

**Validation Checks:**
- Matrix dimension consistency
- Weight value ranges and normalization
- Data type compatibility
- Memory requirements assessment

**Usage Example:**
```smalltalk
validation := visualizer validateAttentionData: attentionMatrix.
validation hasErrors ifTrue: [
    validation errors do: [:error |
        Transcript show: 'Attention data error: ', error description; cr]].
```

#### `diagnoseRenderingPerformance`
Analyzes current rendering performance and provides optimization recommendations.

**Parameters:** None

**Returns:**
- `PerformanceDiagnostic` - Object containing performance analysis and recommendations

**Performance Analysis:**
- Rendering frame rate and timing analysis
- Memory usage patterns and optimization opportunities
- GPU utilization assessment (when available)
- Bottleneck identification and resolution suggestions

**Usage Example:**
```smalltalk
diagnostic := visualizer diagnoseRenderingPerformance.
diagnostic recommendations do: [:recommendation |
    Transcript show: 'Performance tip: ', recommendation description; cr].
diagnostic.frameRate < 30 ifTrue: [
    visualizer enablePerformanceMode: true].
```