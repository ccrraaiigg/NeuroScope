# NeuroScope: A Mechanistic Interpretability Framework

## Core Design Philosophy

NeuroScope leverages Smalltalk's introspective capabilities and message-passing paradigm to create an intuitive framework for transformer interpretability, inspired by TransformerLens. The design emphasizes:

- **Everything is an object**: Activations, hooks, interventions, and analyses are all first-class objects
- **Message-passing for operations**: All computations flow through clean message interfaces
- **Live introspection**: Leverage Smalltalk's reflective capabilities for real-time analysis
- **Interactive exploration**: Built-in inspector integration for immediate feedback

## Core Architecture

### 1. Foundation Classes

```smalltalk
Object subclass: #TransformerModel
    instanceVariableNames: 'layers tokenizer config hooks cache'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Core'

Object subclass: #ActivationTensor
    instanceVariableNames: 'data shape device requiresGrad hooks'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Core'

Object subclass: #Hook
    instanceVariableNames: 'name condition action layer'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Hooks'
```

### 2. Layer Hierarchy

```smalltalk
Object subclass: #TransformerLayer
    instanceVariableNames: 'index model config'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Layers'

TransformerLayer subclass: #AttentionLayer
    instanceVariableNames: 'headCount queryWeights keyWeights valueWeights outputWeights'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Layers'

TransformerLayer subclass: #MLPLayer  
    instanceVariableNames: 'upWeights downWeights activation'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Layers'

TransformerLayer subclass: #EmbeddingLayer
    instanceVariableNames: 'vocabulary embedding positional'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Layers'
```

### 3. Hook System

```smalltalk
Object subclass: #HookManager
    instanceVariableNames: 'activeHooks model'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'NeuroScope-Hooks'

"Hook types"
Hook subclass: #ActivationHook
Hook subclass: #InterventionHook  
Hook subclass: #ProbeHook
Hook subclass: #CachingHook
```

## Key APIs and Usage Patterns

### Model Loading and Basic Operations

```smalltalk
"Load a transformer model"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Basic forward pass"
tokens := model tokenizer encode: 'The cat sat on the'.
output := model forward: tokens.
logits := output logits.

"Generate text"
generated := model generate: 'Hello world' maxTokens: 20.
```

### Activation Extraction

```smalltalk
"Extract activations from specific layers"
tokens := model tokenizer encode: 'The quick brown fox'.

"Method 1: Direct extraction"
activations := model 
    runWithCaching: tokens
    layers: #(0 5 10)
    components: #(#residual #attention #mlp).

"Method 2: Using hooks"
residualHook := ActivationHook 
    layer: 5 
    component: #residual
    action: [:activation | activation copy].

model hookManager addHook: residualHook.
result := model forward: tokens.
layer5Residual := residualHook lastActivation.
```

### Intervention System

```smallttml
"Zero out specific attention heads"
intervention := InterventionHook
    layer: 8
    component: #attention
    heads: #(3 7 11)
    action: [:activation | activation zeroHeads: #(3 7 11)].

model hookManager addHook: intervention.
alteredOutput := model forward: tokens.

"Activation patching"
sourceTokens := model tokenizer encode: 'The cat sat on the mat'.
targetTokens := model tokenizer encode: 'The dog ran in the park'.

"Get clean and corrupted activations"
cleanActivations := model runWithCaching: sourceTokens.
corruptedActivations := model runWithCaching: targetTokens.

"Patch layer 5 residual stream"
patchHook := InterventionHook
    layer: 5
    component: #residual  
    action: [:activation | cleanActivations at: 5 at: #residual].

model hookManager addHook: patchHook.
patchedOutput := model forward: targetTokens.
```

### Analysis and Visualization

```smalltalk
"Attention pattern analysis"
attentionAnalyzer := AttentionAnalyzer for: model.
patterns := attentionAnalyzer 
    analyzeTokens: tokens
    layers: (0 to: 11)
    heads: #all.

patterns inspect. "Opens Smalltalk inspector"

"Probe training"
probe := LinearProbe 
    input: (model activationsAt: #residual layer: 6)
    labels: sentimentLabels
    regularization: 0.01.

probe train.
accuracy := probe evaluate: testData.

"Neuron analysis"
neuronAnalyzer := NeuronAnalyzer for: model layer: 8.
topActivating := neuronAnalyzer 
    findTopActivatingTokens: 1247 "neuron index"
    dataset: commonCrawlSample
    count: 100.
```

### Interactive Exploration

```smalltalk
"Create an interactive lens"
lens := InteractiveLens for: model.
lens openOn: 'The cat sat on the mat'.

"This opens a GUI with:
- Token-by-token visualization
- Layer-by-layer activation viewing  
- Real-time attention pattern display
- Intervention controls
- Component ablation toggles"

"Notebook-style exploration"
notebook := InterpretabilityNotebook new.
notebook model: model.
notebook addCell: (TextCell content: 'Analyzing GPT-2 attention patterns').
notebook addCell: (CodeCell content: 'tokens := model tokenizer encode: ''Hello world''.').
notebook addCell: (VisualizationCell 
    content: 'AttentionVisualizer show: (model attentionPatternsFor: tokens)').
notebook open.
```

## Advanced Features

### Circuit Discovery

```smalltalk
"Automated circuit discovery"
circuitFinder := CircuitFinder for: model.
circuit := circuitFinder
    findCircuitFor: 'indirect object identification'
    examples: indirectObjectExamples
    method: #activationPatching
    threshold: 0.1.

circuit visualize. "Shows computational graph"
circuit components. "Returns involved layers/heads/neurons"
```

### Distributed Analysis

```smalltalk
"Distribute analysis across multiple images/processes"
cluster := ComputeCluster withNodes: #('worker1' 'worker2' 'worker3').

analysis := DistributedAnalysis
    model: model
    cluster: cluster
    task: #attentionPatternAnalysis
    dataset: largeDataset.

results := analysis run.
```

### Custom Metrics and Interventions

```smalltalk
"Define custom intervention"
MyIntervention subclass: #NoiseInjection
    instanceVariableNames: 'noiseLevel'
    
MyIntervention>>apply: activation
    ^ activation + (Tensor randomNormal: activation shape) * noiseLevel

"Define custom metric"  
MyMetric subclass: #AttentionEntropy

MyMetric>>compute: attentionWeights
    ^ attentionWeights collect: [:row | row entropy]
```

## Implementation Considerations

### Performance Optimizations
- **Lazy Evaluation**: Activations computed only when accessed
- **Memory Management**: Automatic cleanup of cached activations
- **Native Bridges**: Interface with optimized tensor libraries
- **Parallel Processing**: Leverage Smalltalk's concurrent capabilities

### Error Handling
```smalltalk
[model forward: tokens] 
    on: InvalidTokenError 
    do: [:error | self handleTokenizationError: error].

[activations at: layerIndex]
    on: SubscriptOutOfBounds
    do: [:error | self raiseLayerNotFoundError: layerIndex].
```

### Testing Framework
```smalltalk
TestCase subclass: #NeuroScopeTest

NeuroScopeTest>>testBasicForwardPass
    | model tokens output |
    model := MockTransformerModel new.
    tokens := #(1 2 3 4 5).
    output := model forward: tokens.
    self assert: output notNil.
    self assert: output logits size equals: tokens size.
```

### Browser Integration Features

```smalltalk
"JavaScript interop for ML computations"
JSInterface class>>loadTensorFlowJS
    <javascript: 'return tf'>

JSInterface class>>createTensor: array shape: shape
    <javascript: 'return tf.tensor(array, shape)'>

"WebGL acceleration"
WebGLTensor subclass: #ActivationTensor
    instanceVariableNames: 'glBuffer context'

WebGLTensor>>multiply: otherTensor
    "Use WebGL shaders for fast matrix operations"
    <javascript: 'return this.context.matmul(this.glBuffer, otherTensor.glBuffer)'>

"Canvas-based visualization"
CanvasRenderer class>>drawAttentionMatrix: matrix on: canvas
    | ctx |
    ctx := canvas getContext: '2d'.
    matrix withIndexDo: [:value :i :j |
        ctx fillStyle: (self colorForValue: value).
        ctx fillRect: i*10 y: j*10 width: 10 height: 10].

"Web Worker integration for heavy processing"
WorkerManager class>>processInBackground: computation
    | worker |
    worker := JSInterface createWorker: 'analysis-worker.js'.
    worker postMessage: computation asJSON.
    ^ worker onMessage: [:result | result parseJSON]
```

### Catalyst-Specific Optimizations

```smalltalk
"Efficient data transfer between Catalyst and JavaScript"
DataBridge class>>transferTensor: tensor
    "Use typed arrays for efficient memory transfer"
    | jsArray |
    jsArray := JSInterface createFloat32Array: tensor size.
    tensor data doWithIndex: [:value :index |
        jsArray at: index put: value].
    ^ jsArray

"Browser storage integration"
BrowserStorage class>>cacheActivations: activations key: key
    | serialized |
    serialized := activations asJSON.
    JSInterface localStorage setItem: key value: serialized.

BrowserStorage class>>loadActivations: key
    | data |
    data := JSInterface localStorage getItem: key.
    ^ data ifNotNil: [ActivationTensor fromJSON: data]

"Real-time visualization updates"
RealtimeVisualizer>>updateDisplay: newData
    "Use requestAnimationFrame for smooth updates"
    JSInterface requestAnimationFrame: [
        self canvas clear.
        self drawVisualization: newData].
```

### External Libraries
- **JavaScript Bridge**: Direct integration with web ML libraries (TensorFlow.js, WebGL compute)
- **Canvas/SVG**: Native browser graphics for visualizations
- **WebGL**: GPU-accelerated tensor operations through browser APIs
- **IndexedDB**: Browser-based storage for caching activation datasets
- **Web Workers**: Background processing for heavy computations

### File Formats
- **JSON**: Configuration, metadata, and model serialization
- **Browser Local Storage**: Quick caching of small datasets
- **IndexedDB**: Structured storage for large activation tensors
- **Blob URLs**: Efficient handling of tensor data in browser

## Benefits of the Catalyst Approach

1. **Zero Installation**: Run complex interpretability analysis directly in browser
2. **Live Development**: Modify analysis code while experiments run, with instant browser updates
3. **Powerful Introspection**: Every object inspectable at runtime with browser dev tools integration
4. **Web-Native Graphics**: Leverage Canvas, WebGL, and SVG for rich visualizations
5. **JavaScript Ecosystem**: Direct access to TensorFlow.js, D3.js, and other web ML tools
6. **Cross-Platform**: Works on any device with a modern browser
7. **Shareable**: Send complete analysis environments via URL
8. **GPU Acceleration**: WebGL compute shaders for tensor operations

## Example Research Workflow

```smalltalk
"1. Load model and data"
model := TransformerModel fromHuggingFace: 'gpt2-medium'.
dataset := Dataset fromFile: 'fact_retrieval_examples.json'.

"2. Exploratory analysis"
lens := InteractiveLens for: model.
dataset examples do: [:example |
    lens analyzeText: example text].

"3. Hypothesis formation"
hypothesis := 'Layer 16 head 8 performs subject extraction'.

"4. Automated testing"  
tester := HypothesisTester 
    model: model
    hypothesis: hypothesis
    dataset: dataset.
    
results := tester run.

"5. Publication preparation"
paper := ResearchPaper new
    title: 'Subject Extraction in GPT-2'
    addResults: results
    addVisualizations: lens exportedFigures.
    
paper generateLatex.
```

This framework combines the analytical power of TransformerLens with Smalltalk's unique strengths in interactive development and object introspection.
