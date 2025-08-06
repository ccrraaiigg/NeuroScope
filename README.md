# NeuroScope

A mechanistic interpretability framework for transformer models, built in Smalltalk and designed to run in SqueakJS browsers. NeuroScope provides an interactive, object-oriented approach to understanding how transformer neural networks work internally.

## 🚀 Key Features

- **Zero-installation browser-based analysis** - Run complex interpretability experiments directly in web browsers
- **Interactive exploration** - Live introspection of model components with real-time visualization  
- **Object-oriented design** - Everything (activations, hooks, interventions) is a first-class object
- **Circuit discovery** - Automated identification of computational pathways in transformers
- **Web-native graphics** - Leverage Canvas, WebGL, and SVG for rich visualizations
- **GPU acceleration** - WebGL compute shaders for efficient tensor operations

## 🎯 Target Users

- ML researchers studying transformer interpretability
- Educators teaching neural network concepts
- Developers building interpretability tools
- Anyone wanting to understand how language models work internally

## 🏗️ Architecture

NeuroScope combines the analytical power of existing interpretability frameworks with Smalltalk's unique strengths in interactive development, live object introspection, and browser-based deployment.

### Core Components

- **TransformerModel** - Main model interface with HuggingFace integration
- **ActivationTensor** - First-class activation objects with GPU acceleration
- **Hook System** - Flexible intervention and monitoring framework
- **Interactive Lens** - Real-time visualization and exploration tools
- **Circuit Finder** - Automated discovery of computational pathways

## 🚀 Quick Start

### Basic Model Loading

```smalltalk
"Load a pre-trained model from HuggingFace"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Basic forward pass"
tokens := model tokenizer encode: 'The cat sat on the mat'.
output := model forward: tokens.
logits := output logits.

"Generate text"
generated := model generate: 'Hello world' maxTokens: 20.
```

### Activation Analysis

```smalltalk
"Extract activations from specific layers"
tokens := model tokenizer encode: 'The quick brown fox'.

"Cache activations from multiple layers"
result := model 
    runWithCaching: tokens
    layers: #(0 5 10)
    components: #(#residual #attention #mlp).

"Access cached activations"
layer5Residual := result cachedActivations at: (5 -> #residual).
```

### Hook System

```smalltalk
"Create monitoring hook"
monitorHook := ActivationHook 
    layer: 5 
    component: #residual
    action: [:activation | 
        Transcript show: 'Layer 5 mean: ', activation mean asString.
        activation
    ].

model hookManager addHook: monitorHook.
output := model forward: tokens.
```

### Interactive Exploration

```smalltalk
"Create an interactive lens for real-time analysis"
lens := InteractiveLens for: model.
lens openOn: 'The cat sat on the mat'.

"This opens a GUI with:
- Token-by-token visualization
- Layer-by-layer activation viewing  
- Real-time attention pattern display
- Intervention controls"
```

## 🔧 Technology Stack

### Core Platform
- **Smalltalk** - Primary development language with live introspection
- **SqueakJS** - Browser-based Smalltalk environment for zero-installation deployment
- **JavaScript Bridge** - Direct integration with web ML libraries

### ML/Tensor Libraries
- **TensorFlow.js** - Browser-based tensor operations and model loading
- **WebGL** - GPU-accelerated tensor computations
- **HuggingFace Integration** - Model loading from HuggingFace Hub

### Visualization & Graphics
- **Canvas API** - 2D graphics and real-time visualizations
- **WebGL** - GPU-accelerated graphics and compute shaders
- **SVG** - Vector graphics for publication-quality figures
- **D3.js** - Advanced data visualization capabilities

## 📁 Project Structure

```
NeuroScope/
├── classes/                    # Smalltalk class definitions
│   ├── TransformerModel/      # Core model classes
│   ├── ActivationTensor/      # Tensor operations
│   ├── Hook/                  # Hook system base
│   ├── AttentionLayer/        # Layer implementations
│   └── InteractiveLens/       # Visualization tools
├── .kiro/                     # Kiro IDE configuration
│   ├── specs/                 # Feature specifications
│   └── steering/              # Development guidelines
├── glossary.md                # Key concepts and terminology
├── summary.md                 # Detailed architecture overview
├── usage-examples.md          # Comprehensive code examples
└── README.md                  # This file
```

## 🎨 Advanced Features

### Circuit Discovery

```smalltalk
"Automated circuit discovery"
circuitFinder := CircuitFinder for: model.
circuit := circuitFinder
    findCircuitFor: 'indirect object identification'
    examples: indirectObjectExamples
    method: #activationPatching
    threshold: 0.1.

circuit visualize.  "Shows computational graph"
```

### Attention Analysis

```smalltalk
"Attention pattern analysis"
attentionAnalyzer := AttentionAnalyzer for: model.
patterns := attentionAnalyzer 
    analyzeTokens: tokens
    layers: (0 to: 11)
    heads: #all.

patterns inspect.  "Opens Smalltalk inspector"
```

### Probing and Interventions

```smalltalk
"Linear probe training"
probe := LinearProbe 
    input: (model activationsAt: #residual layer: 6)
    labels: sentimentLabels
    regularization: 0.01.

probe train.
accuracy := probe evaluate: testData.

"Activation patching"
patchHook := InterventionHook
    layer: 5
    component: #residual  
    action: [:activation | cleanActivations at: 5 at: #residual].

model hookManager addHook: patchHook.
patchedOutput := model forward: corruptedTokens.
```

## 🌐 Browser Integration

NeuroScope leverages modern web technologies for maximum accessibility:

- **WebGL Compute** - GPU-accelerated tensor operations
- **Canvas Rendering** - Real-time visualization updates
- **IndexedDB Storage** - Efficient caching of large activation datasets
- **Web Workers** - Background processing for heavy computations
- **Responsive Design** - Works on desktop, tablet, and mobile devices

## 🧪 Development Workflow

### Common Commands

```smalltalk
"Load the development environment"
NeuroScope loadDevelopmentTools.

"Run all tests"
NeuroScopeTest suite run.

"Open interactive development workspace"
Workspace open.

"Performance benchmarking"
Benchmark run: [model forward: tokens] iterations: 100.
```

### Testing

```smalltalk
"Run specific test category"
(TestSuite named: 'NeuroScope-Core') run.

"Validate model loading"
ModelValidator validateModel: model.
```

## 📊 Performance

NeuroScope is optimized for browser environments:

- **Lazy Evaluation** - Activations computed only when accessed
- **Memory Management** - Automatic cleanup of cached activations
- **Parallel Processing** - Leverage Web Workers for heavy computations
- **GPU Acceleration** - WebGL shaders for tensor operations

## 🤝 Contributing

We welcome contributions! Please see our development guidelines in `.kiro/steering/` for:

- Code style conventions
- Testing requirements
- Documentation standards
- Performance considerations

## 📄 License

This project is licensed under the terms specified in the LICENSE file.

## 🔗 Related Projects

- **TransformerLens** - Python-based mechanistic interpretability framework
- **Anthropic's Interpretability Research** - Cutting-edge interpretability techniques
- **OpenAI's Microscope** - Neural network visualization tools

## 📚 Documentation

- [`summary.md`](summary.md) - Detailed architecture and design philosophy
- [`usage-examples.md`](usage-examples.md) - Comprehensive code examples
- [`glossary.md`](glossary.md) - Key concepts and terminology
- [`mechanistic-interpretability-overview.md`](mechanistic-interpretability-overview.md) - Field overview

## 🎓 Learning Resources

NeuroScope is designed to be educational. Start with:

1. Load a small model (GPT-2 small)
2. Run basic forward passes
3. Explore activations with the Interactive Lens
4. Try simple interventions with hooks
5. Discover circuits with automated tools

The framework's object-oriented design makes complex interpretability concepts accessible through familiar programming patterns.

---

**NeuroScope: Making transformer interpretability accessible, interactive, and insightful.**