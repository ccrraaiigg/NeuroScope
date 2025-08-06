# Project Structure

## Package Organization

The NeuroScope framework follows Smalltalk's package-based organization with clear separation of concerns:

### Core Packages
- **NeuroScope-Core** - Foundation classes (TransformerModel, ActivationTensor, base abstractions)
- **NeuroScope-Layers** - Layer hierarchy (TransformerLayer, AttentionLayer, MLPLayer, EmbeddingLayer)
- **NeuroScope-Hooks** - Hook system (HookManager, ActivationHook, InterventionHook, ProbeHook)
- **NeuroScope-Analysis** - Analysis tools (AttentionAnalyzer, NeuronAnalyzer, CircuitFinder)
- **NeuroScope-Visualization** - Graphics and UI (InteractiveLens, CanvasRenderer, AttentionVisualizer)
- **NeuroScope-Tests** - Test suite (unit tests, integration tests, benchmarks)

### Specialized Packages
- **NeuroScope-Browser** - Browser-specific integrations (JSInterface, WebGLTensor, BrowserStorage)
- **NeuroScope-Probes** - Probing and intervention tools (LinearProbe, ActivationPatching)
- **NeuroScope-Utils** - Utilities and helpers (DataBridge, ModelValidator, Benchmark)

## Class Hierarchy Patterns

### Foundation Pattern
```smalltalk
Object subclass: #BaseClass
    instanceVariableNames: 'commonAttributes'
    category: 'NeuroScope-Core'
```

### Layer Hierarchy
```smalltalk
TransformerLayer (abstract base)
├── AttentionLayer
├── MLPLayer
└── EmbeddingLayer
```

### Hook System
```smalltalk
Hook (abstract base)
├── ActivationHook
├── InterventionHook
├── ProbeHook
└── CachingHook
```

## Naming Conventions

### Classes
- **PascalCase** for class names (TransformerModel, ActivationTensor)
- **Descriptive suffixes** indicating purpose (Analyzer, Visualizer, Manager, Hook)
- **Abstract base classes** use generic names (Layer, Hook, Tensor)

### Methods
- **camelCase** for method names (forward:, addHook:, analyzeTokens:)
- **Keyword messages** for multi-parameter methods (runWithCaching:layers:components:)
- **Boolean methods** end with question marks (requiresGrad?, isActive?)
- **Mutating methods** use imperative verbs (add:, remove:, clear)

### Instance Variables
- **camelCase** for instance variable names
- **Descriptive names** over abbreviations (tokenizer not tok, activation not act)
- **Collections** use plural forms (hooks, layers, activations)

## File Organization

### Configuration Files
- **JSON format** for all configuration and metadata
- **Descriptive filenames** (model_config.json, analysis_settings.json)

### Test Organization
- **One test class per production class** when possible
- **Integration tests** in separate test classes
- **Mock objects** for external dependencies

## Code Style Guidelines

### Method Structure
```smalltalk
methodName: parameter
    "Brief description of what the method does"
    | localVariable |
    
    "Guard clauses first"
    parameter ifNil: [^self].
    
    "Main logic"
    localVariable := self computeSomething: parameter.
    
    "Return value"
    ^localVariable
```

### Error Handling
- **Use exception handling** for recoverable errors
- **Descriptive error messages** with context
- **Fail fast** for programming errors

### Documentation
- **Method comments** for public APIs
- **Class comments** explaining purpose and usage
- **Package comments** describing overall functionality

## Browser Integration Patterns

### JavaScript Interop
```smalltalk
JSInterface class>>methodName
    <javascript: 'return nativeJSCode()'>
```

### Canvas/WebGL Integration
- **Separate rendering classes** for different graphics backends
- **Abstract graphics interface** for platform independence
- **Efficient data transfer** using typed arrays

### Storage Patterns
- **Lazy loading** for large datasets
- **Caching strategies** for frequently accessed data
- **Cleanup methods** for memory management

## Testing Structure

### Unit Tests
- **Test one class/method** per test method
- **Descriptive test names** (testBasicForwardPass, testHookActivation)
- **Setup and teardown** methods for common initialization

### Integration Tests
- **End-to-end workflows** testing multiple components
- **Mock external dependencies** (models, datasets)
- **Performance benchmarks** for critical paths