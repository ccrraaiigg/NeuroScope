# Technology Stack

## Core Platform
- **Smalltalk** - Primary development language leveraging object-oriented design and live introspection
- **SqueakJS** - Browser-based Smalltalk environment for zero-installation deployment
- **JavaScript Bridge** - Direct integration with web ML libraries and browser APIs

## ML/Tensor Libraries
- **TensorFlow.js** - Browser-based tensor operations and model loading
- **WebGL** - GPU-accelerated tensor computations through browser APIs
- **HuggingFace Integration** - Model loading from HuggingFace Hub

## Visualization & Graphics
- **Canvas API** - 2D graphics and real-time visualizations
- **WebGL** - GPU-accelerated graphics and compute shaders
- **SVG** - Vector graphics for publication-quality figures
- **D3.js** - Advanced data visualization capabilities

## Storage & Data
- **JSON** - Configuration, metadata, and model serialization
- **IndexedDB** - Browser-based storage for large activation datasets
- **Local Storage** - Quick caching of small datasets
- **Blob URLs** - Efficient tensor data handling

## Performance & Concurrency
- **Web Workers** - Background processing for heavy computations
- **RequestAnimationFrame** - Smooth real-time visualization updates
- **Typed Arrays** - Efficient memory transfer between Smalltalk and JavaScript
- **Lazy Evaluation** - Activations computed only when accessed

## Common Commands

### Development
```smalltalk
"Load the development environment"
NeuroScope loadDevelopmentTools.

"Run all tests"
NeuroScopeTest suite run.

"Open interactive development workspace"
Workspace open.
```

### Model Operations
```smalltalk
"Load a model from HuggingFace"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Run basic forward pass"
tokens := model tokenizer encode: 'Hello world'.
output := model forward: tokens.

"Start interactive analysis"
lens := InteractiveLens for: model.
lens openOn: 'Your text here'.
```

### Testing & Validation
```smalltalk
"Run specific test category"
(TestSuite named: 'NeuroScope-Core') run.

"Validate model loading"
ModelValidator validateModel: model.

"Performance benchmarking"
Benchmark run: [model forward: tokens] iterations: 100.
```

## Browser Requirements
- Modern browser with WebGL support
- JavaScript enabled
- Minimum 4GB RAM recommended for medium-sized models
- GPU acceleration recommended for large models