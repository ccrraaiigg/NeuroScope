# JSInterface Class Documentation

## Class Comment

JSInterface serves as the primary bridge between Smalltalk and JavaScript environments, enabling seamless interoperability for web-based neural network analysis. This class provides the foundational infrastructure for calling JavaScript functions from Smalltalk, transferring data between environments, and managing browser-specific functionality within the NeuroScope framework.

### Purpose and Responsibilities

The JSInterface class manages the complex task of bridging two fundamentally different runtime environments. It handles type conversion, memory management, and execution context switching required for Smalltalk objects to interact with JavaScript APIs, web libraries, and browser services.

Key responsibilities include:
- **Bidirectional Method Invocation**: Execute JavaScript functions from Smalltalk and vice versa
- **Type System Bridging**: Convert between Smalltalk objects and JavaScript primitives/objects
- **Memory Management**: Handle object lifecycle across language boundaries
- **Error Propagation**: Translate exceptions and errors between environments
- **Browser API Access**: Provide Smalltalk access to web APIs (DOM, Canvas, WebGL, Storage)

### Instance Variables

- **jsContext**: Reference to the JavaScript global context for function execution
- **typeConverters**: Dictionary mapping Smalltalk types to JavaScript conversion functions
- **callbackRegistry**: Registry of Smalltalk blocks that can be called from JavaScript
- **errorHandler**: Strategy object for handling cross-language exceptions
- **apiBindings**: Cache of frequently used JavaScript API references

### JavaScript Bridge Patterns

#### Basic Function Invocation
```smalltalk
"Call a simple JavaScript function"
result := JSInterface call: 'Math.sqrt' with: 16.
"Returns: 4"

"Call with multiple parameters"
result := JSInterface 
    call: 'console.log' 
    withArguments: #('Debug message:' 42 true).
```

#### Object Method Invocation
```smalltalk
"Create and manipulate JavaScript objects"
canvas := JSInterface eval: 'document.createElement("canvas")'.
JSInterface 
    call: 'setAttribute' 
    on: canvas 
    withArguments: #('width' '800').

context := JSInterface call: 'getContext' on: canvas with: '2d'.
```

#### Callback Registration
```smalltalk
"Register Smalltalk block as JavaScript callback"
callback := [:event | 
    Transcript show: 'Mouse clicked at: ', event x asString, ',', event y asString].

JSInterface 
    registerCallback: callback 
    as: 'mouseClickHandler'.

"Use in JavaScript: mouseClickHandler(event)"
```

### Data Transfer Examples

#### Primitive Type Conversion
```smalltalk
"Smalltalk to JavaScript"
JSInterface convertToJS: 42.           "→ JavaScript number"
JSInterface convertToJS: 'hello'.      "→ JavaScript string"  
JSInterface convertToJS: true.         "→ JavaScript boolean"
JSInterface convertToJS: #(1 2 3).     "→ JavaScript array"

"JavaScript to Smalltalk"
jsNumber := JSInterface eval: '42'.
smalltalkNumber := JSInterface convertFromJS: jsNumber.
```

#### Complex Object Transfer
```smalltalk
"Transfer tensor data efficiently"
tensor := ActivationTensor withData: #(1.0 2.0 3.0 4.0) shape: #(2 2).
jsTypedArray := JSInterface convertTensorToTypedArray: tensor.

"Transfer back with type preservation"
reconstructedTensor := JSInterface convertTypedArrayToTensor: jsTypedArray.
```

#### JSON Serialization
```smalltalk
"Complex object serialization"
config := Dictionary new
    at: 'modelName' put: 'gpt2-small';
    at: 'layers' put: 12;
    at: 'hiddenSize' put: 768;
    yourself.

jsonString := JSInterface toJSON: config.
JSInterface call: 'localStorage.setItem' withArguments: #('config' jsonString).
```

### Browser Compatibility Requirements

#### Minimum Browser Support
- **Chrome/Chromium**: Version 80+ (required for WebGL 2.0 and modern JavaScript features)
- **Firefox**: Version 75+ (required for SharedArrayBuffer and WebGL extensions)
- **Safari**: Version 13+ (required for BigInt and modern Canvas features)
- **Edge**: Version 80+ (Chromium-based versions only)

#### Required Browser Features
```smalltalk
"Feature detection and validation"
JSInterface validateBrowserSupport
    checkFeature: 'WebGL2RenderingContext';
    checkFeature: 'SharedArrayBuffer';
    checkFeature: 'OffscreenCanvas';
    checkFeature: 'BigInt';
    checkFeature: 'WebAssembly';
    yourself.
```

#### Progressive Enhancement
```smalltalk
"Graceful degradation for unsupported features"
webglSupported := JSInterface hasFeature: 'WebGL2RenderingContext'.
webglSupported 
    ifTrue: [self useWebGLAcceleration]
    ifFalse: [self useFallbackRendering].
```

### Performance Considerations

#### Minimizing Bridge Overhead
```smalltalk
"Batch operations to reduce crossing overhead"
operations := Array new: 1000.
1 to: 1000 do: [:i | 
    operations at: i put: (Array with: 'setValue' with: i with: (i * 2))].

JSInterface batchExecute: operations.
```

#### Memory Management
```smalltalk
"Explicit cleanup of JavaScript references"
jsObject := JSInterface eval: 'new LargeObject()'.
[
    "Use jsObject for computations"
    self performAnalysisWith: jsObject.
] ensure: [
    JSInterface releaseReference: jsObject.
].
```

### Error Handling and Debugging

#### Exception Translation
```smalltalk
"Handle JavaScript errors in Smalltalk"
[
    result := JSInterface call: 'riskyFunction' with: parameter.
] on: JSError do: [:error |
    Transcript show: 'JavaScript error: ', error messageText.
    "Provide fallback behavior"
    result := self fallbackComputation: parameter.
].
```

#### Debug Mode Integration
```smalltalk
"Enable detailed logging for development"
JSInterface enableDebugMode.
JSInterface logLevel: #detailed.

"All bridge calls will now log timing and data transfer information"
result := JSInterface call: 'complexFunction' with: data.
```

### Integration with NeuroScope Components

#### Model Loading Integration
```smalltalk
"Load models using JavaScript ML libraries"
modelUrl := 'https://huggingface.co/gpt2/resolve/main/model.json'.
jsModel := JSInterface 
    call: 'tf.loadLayersModel' 
    with: modelUrl.

smalltalkModel := TransformerModel fromJSModel: jsModel.
```

#### Visualization Bridge
```smalltalk
"Connect Smalltalk analysis with JavaScript visualization"
attentionPatterns := model analyzeAttention: tokens.
jsPatterns := JSInterface convertToJS: attentionPatterns.

JSInterface 
    call: 'renderAttentionVisualization' 
    withArguments: #(jsPatterns 'attention-canvas').
```

### Browser Limitations and Workarounds

#### Memory Constraints
- **32-bit Limitation**: JavaScript numbers limited to 32-bit integers for bitwise operations
- **Heap Size**: Browser heap typically limited to 2-4GB depending on system
- **Garbage Collection**: No direct control over GC timing, may cause visualization stutters

#### Security Restrictions
- **CORS Policy**: Cross-origin requests require proper headers for model loading
- **Local File Access**: File:// URLs have restricted capabilities
- **SharedArrayBuffer**: Requires secure context (HTTPS) and specific headers

#### Workaround Strategies
```smalltalk
"Handle CORS issues with proxy"
JSInterface configureCORSProxy: 'https://cors-anywhere.herokuapp.com/'.

"Detect secure context for advanced features"
secureContext := JSInterface eval: 'window.isSecureContext'.
secureContext ifFalse: [
    self showSecurityWarning.
    self disableAdvancedFeatures.
].
```

### Best Practices

1. **Minimize Bridge Crossings**: Batch operations when possible to reduce overhead
2. **Explicit Memory Management**: Always release JavaScript object references
3. **Type Safety**: Use type conversion methods rather than direct casting
4. **Error Handling**: Always wrap bridge calls in exception handlers
5. **Feature Detection**: Check browser capabilities before using advanced features
6. **Performance Monitoring**: Use built-in timing methods to identify bottlenecks

### Common Usage Patterns

#### Initialization Pattern
```smalltalk
"Standard JSInterface setup"
JSInterface initialize.
JSInterface loadRequiredLibraries: #('tensorflow' 'd3' 'plotly').
JSInterface validateEnvironment.
```

#### Cleanup Pattern
```smalltalk
"Proper resource cleanup"
JSInterface class>>shutDown
    self releaseAllReferences.
    self clearCallbackRegistry.
    self resetTypeConverters.
```

This documentation provides comprehensive coverage of JavaScript interoperability patterns, browser compatibility requirements, and practical examples for integrating Smalltalk with web technologies in the NeuroScope framework.