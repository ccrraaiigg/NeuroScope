# TransformerModel Class Comment

TransformerModel serves as the central orchestrator for transformer neural network operations within the NeuroScope framework. This class encapsulates the complete model architecture, managing the coordination between layers, tokenization, configuration, hooks, and caching systems.

## Responsibilities

The TransformerModel class is responsible for:
- **Model Architecture Management**: Coordinating the sequence of transformer layers and their interactions
- **Forward Pass Orchestration**: Managing the flow of activations through the complete model pipeline
- **Hook System Integration**: Providing attachment points for analysis, intervention, and caching hooks
- **Tokenization Interface**: Bridging between text input and numerical token representations
- **Configuration Management**: Maintaining model hyperparameters and architectural settings
- **Activation Caching**: Optimizing repeated computations through intelligent caching strategies

## Instance Variables

- **layers**: An OrderedCollection of TransformerLayer instances representing the model's computational stack
- **tokenizer**: A Tokenizer instance responsible for text-to-token and token-to-text conversion
- **config**: A Dictionary containing model configuration parameters (hidden size, attention heads, etc.)
- **hooks**: A HookManager instance coordinating all attached analysis and intervention hooks
- **cache**: An ActivationCache instance storing computed activations for performance optimization

## Architecture Overview

TransformerModel follows a layered architecture where each layer processes activations sequentially. The model supports both standard forward passes and instrumented execution with hooks for interpretability analysis.

```
Input Text → Tokenizer → Embedding → Layer₀ → Layer₁ → ... → LayerN → Output Logits
                                      ↓         ↓              ↓
                                   Hooks    Hooks          Hooks
                                      ↓         ↓              ↓
                                   Cache    Cache          Cache
```

## Usage Patterns

### Basic Model Loading and Forward Pass

```smalltalk
"Load a pre-trained model from HuggingFace"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Perform basic text processing"
tokens := model tokenizer encode: 'The cat sat on the mat'.
output := model forward: tokens.
logits := output logits.
probabilities := logits softmax.
```

### Text Generation

```smalltalk
"Generate continuation of input text"
generated := model 
    generate: 'Once upon a time' 
    maxTokens: 50 
    temperature: 0.8.

"Beam search generation"
beams := model 
    generateWithBeamSearch: 'The future of AI is'
    beamWidth: 5
    maxTokens: 30.
```

### Instrumented Execution with Hooks

```smalltalk
"Extract activations from specific layers"
model hookManager addHook: (ActivationHook 
    layer: 6 
    component: #residual
    action: [:activation | activation copy]).

result := model forward: tokens.
layer6Activations := model hookManager hookNamed: 'layer6-residual' lastActivation.

"Intervention during forward pass"
model hookManager addHook: (InterventionHook
    layer: 8
    component: #attention
    action: [:activation | activation zeroHeads: #(2 5 9)]).

modifiedOutput := model forward: tokens.
```

### Batch Processing

```smalltalk
"Process multiple sequences efficiently"
tokenBatches := sentences collect: [:sentence | model tokenizer encode: sentence].
batchedOutput := model forwardBatch: tokenBatches.

"Extract activations for entire batch"
batchActivations := model 
    runWithCaching: tokenBatches
    layers: #(0 6 12)
    components: #(#residual #attention #mlp).
```

## Integration with Analysis Framework

TransformerModel serves as the foundation for all interpretability analyses in NeuroScope:

- **Attention Analysis**: Provides access to attention weights and patterns across all layers
- **Activation Patching**: Enables systematic intervention experiments
- **Probe Training**: Supplies activation data for linear probe experiments  
- **Circuit Discovery**: Supports automated identification of computational pathways
- **Interactive Exploration**: Powers real-time visualization and manipulation interfaces

## Performance Considerations

- **Lazy Evaluation**: Activations are computed only when explicitly requested or cached
- **Memory Management**: Automatic cleanup of cached activations based on usage patterns
- **Hook Optimization**: Minimal overhead when no hooks are attached
- **Batch Processing**: Efficient handling of multiple sequences through vectorized operations

## Error Handling

The class provides comprehensive error handling for common failure modes:
- Invalid token sequences
- Layer index out of bounds
- Configuration mismatches
- Memory allocation failures
- Hook execution errors

## Browser Integration

Via SqueakJS, TransformerModel leverages browser-specific optimizations:
- WebGL acceleration for tensor operations
- Web Workers for background processing
- IndexedDB for persistent activation caching
- Canvas integration for real-time visualization

## Method Documentation

### Class Methods

#### fromHuggingFace: modelName
**Purpose**: Creates a new TransformerModel instance by loading a pre-trained model from the HuggingFace Hub.

**Parameters**:
- `modelName` (String): The HuggingFace model identifier (e.g., 'gpt2-small', 'bert-base-uncased')

**Return Value**: A fully initialized TransformerModel instance with loaded weights and configuration

**Usage Examples**:
```smalltalk
"Load GPT-2 small model"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Load BERT base model"
bertModel := TransformerModel fromHuggingFace: 'bert-base-uncased'.

"Load custom fine-tuned model"
customModel := TransformerModel fromHuggingFace: 'username/my-fine-tuned-model'.
```

**Error Conditions**:
- Raises `ModelNotFoundError` if the specified model doesn't exist on HuggingFace
- Raises `NetworkError` if unable to download model files
- Raises `ConfigurationError` if model configuration is invalid or unsupported

**Performance Notes**: Initial model loading may take several seconds depending on model size and network speed. Consider showing progress indicators for large models.

### Instance Methods

#### forward: tokens
**Purpose**: Performs a complete forward pass through the transformer model, computing output logits for the given input tokens.

**Parameters**:
- `tokens` (Array of Integer): Token IDs representing the input sequence, typically obtained from the tokenizer

**Return Value**: A ModelOutput object containing:
- `logits`: ActivationTensor with shape [sequence_length, vocabulary_size]
- `hiddenStates`: Array of ActivationTensor objects for each layer (if requested)
- `attentions`: Array of attention weight tensors (if requested)

**Side Effects**: 
- Executes all registered hooks during the forward pass
- Updates activation cache if caching is enabled
- May modify model state through intervention hooks

**Usage Examples**:
```smalltalk
"Basic forward pass"
tokens := model tokenizer encode: 'Hello world'.
output := model forward: tokens.
nextTokenLogits := output logits last.

"Get probabilities for next token prediction"
probabilities := nextTokenLogits softmax.
topTokens := probabilities topK: 10.

"Access hidden states if available"
output hiddenStates ifNotNil: [
    finalLayerHidden := output hiddenStates last.
    embedding := finalLayerHidden mean  "Average pooling"
].
```

**Performance Notes**: Computational complexity is O(n²d) where n is sequence length and d is model dimension due to attention operations.

#### generate: prompt maxTokens: maxTokens
**Purpose**: Generates text continuation using autoregressive sampling from the model's learned distribution.

**Parameters**:
- `prompt` (String): The initial text to continue generating from
- `maxTokens` (Integer): Maximum number of new tokens to generate

**Return Value**: A String containing the original prompt plus the generated continuation

**Side Effects**: 
- Performs multiple forward passes (one per generated token)
- May trigger hooks on each forward pass
- Updates internal generation state

**Usage Examples**:
```smalltalk
"Simple text generation"
generated := model 
    generate: 'The future of artificial intelligence is' 
    maxTokens: 50.

"Generate with custom parameters"
story := model 
    generate: 'Once upon a time in a distant galaxy'
    maxTokens: 100
    temperature: 0.8
    topP: 0.9.

"Generate multiple completions"
completions := (1 to: 5) collect: [:i |
    model generate: prompt maxTokens: 30
].
```

**Error Conditions**:
- Raises `TokenizationError` if prompt cannot be tokenized
- Raises `GenerationError` if generation fails due to model constraints
- May produce degraded output if maxTokens exceeds model's context window

**Performance Notes**: Generation time scales linearly with maxTokens. Consider using beam search for higher quality at the cost of increased computation.

#### runWithCaching: tokens layers: layerIndices components: componentNames
**Purpose**: Executes a forward pass while caching activations from specified layers and components for later analysis.

**Parameters**:
- `tokens` (Array of Integer): Input token sequence
- `layerIndices` (Array of Integer): Layer indices to cache (0-based indexing)
- `componentNames` (Array of Symbol): Component names to cache (#residual, #attention, #mlp, #layerNorm)

**Return Value**: A CachedModelOutput containing:
- Standard forward pass results (logits, etc.)
- `cachedActivations`: Dictionary mapping (layer, component) pairs to ActivationTensor objects
- `cacheMetadata`: Information about cache size and computation time

**Side Effects**:
- Populates the model's activation cache
- May consume significant memory for large models or long sequences
- Executes any registered hooks in addition to caching

**Usage Examples**:
```smalltalk
"Cache residual stream activations from multiple layers"
result := model 
    runWithCaching: tokens
    layers: #(0 6 12 18)
    components: #(#residual).

residualActivations := result cachedActivations.
layer6Residual := residualActivations at: (6 -> #residual).

"Cache attention patterns for analysis"
attentionResult := model
    runWithCaching: tokens  
    layers: #(8 9 10)
    components: #(#attention #attentionWeights).

attentionWeights := attentionResult cachedActivations at: (9 -> #attentionWeights).
headPatterns := attentionWeights splitByHeads.

"Comprehensive caching for circuit analysis"
fullResult := model
    runWithCaching: tokens
    layers: (0 to: model numLayers - 1)
    components: #(#residual #attention #mlp #layerNorm).

"Analyze information flow between components"
mlpOutputs := fullResult cachedActivations select: [:key :value | 
    key value = #mlp
].
```

**Error Conditions**:
- Raises `InvalidLayerError` if any layer index is out of bounds
- Raises `UnknownComponentError` if component name is not recognized
- Raises `MemoryError` if caching would exceed available memory

**Performance Notes**: 
- Memory usage scales with O(layers × sequence_length × hidden_dimension)
- Consider caching only necessary components to minimize memory footprint
- Cache cleanup occurs automatically but can be triggered manually with `clearCache`

#### addHook: hook
**Purpose**: Registers a hook to be executed during forward passes for analysis or intervention purposes.

**Parameters**:
- `hook` (Hook): A Hook instance specifying when and how to intercept model execution

**Return Value**: The hook instance (for method chaining)

**Side Effects**: 
- Adds hook to the model's hook manager
- Hook will be executed on subsequent forward passes
- May impact model performance depending on hook complexity

**Usage Examples**:
```smalltalk
"Add activation extraction hook"
activationHook := ActivationHook 
    layer: 8 
    component: #attention
    action: [:activation | activation copy].
model addHook: activationHook.

"Add intervention hook to zero specific attention heads"
interventionHook := InterventionHook
    layer: 10
    component: #attention  
    action: [:activation | activation zeroHeads: #(2 5 8)].
model addHook: interventionHook.

"Add probe hook for linear probe training"
probeHook := ProbeHook
    layer: 12
    component: #residual
    probe: myLinearProbe.
model addHook: probeHook.

"Method chaining for multiple hooks"
model 
    addHook: (ActivationHook layer: 6 component: #mlp);
    addHook: (CachingHook layer: 6 component: #residual);
    addHook: (InterventionHook layer: 8 component: #attention).
```

**Performance Notes**: Each hook adds computational overhead. Remove unused hooks with `removeHook:` or `clearHooks` to maintain performance.

#### removeHook: hook
**Purpose**: Unregisters a previously added hook from the model's execution pipeline.

**Parameters**:
- `hook` (Hook): The hook instance to remove

**Return Value**: Boolean indicating whether the hook was found and removed

**Usage Examples**:
```smalltalk
"Remove specific hook"
success := model removeHook: myActivationHook.

"Remove hook by name"
model removeHookNamed: 'layer8-attention-extraction'.

"Conditional hook removal"
model hooks do: [:hook |
    hook layer = 6 ifTrue: [model removeHook: hook]
].
```

#### clearHooks
**Purpose**: Removes all registered hooks from the model, returning it to standard execution mode.

**Return Value**: Self (for method chaining)

**Side Effects**: All hooks are unregistered and will no longer execute during forward passes

**Usage Examples**:
```smalltalk
"Clean slate for new analysis"
model clearHooks.

"Reset after intervention experiment"
model 
    clearHooks;
    forward: tokens.  "Clean forward pass"
```

#### tokenizer
**Purpose**: Provides access to the model's tokenizer for text-to-token conversion.

**Return Value**: The Tokenizer instance associated with this model

**Usage Examples**:
```smalltalk
"Encode text to tokens"
tokens := model tokenizer encode: 'Hello, world!'.

"Decode tokens back to text"  
text := model tokenizer decode: tokens.

"Get vocabulary information"
vocabSize := model tokenizer vocabularySize.
specialTokens := model tokenizer specialTokens.
```

#### config
**Purpose**: Provides access to the model's configuration parameters.

**Return Value**: Dictionary containing model hyperparameters and architectural settings

**Usage Examples**:
```smalltalk
"Access model dimensions"
hiddenSize := model config at: #hiddenSize.
numLayers := model config at: #numLayers.
numHeads := model config at: #numAttentionHeads.

"Check model capabilities"
maxPosition := model config at: #maxPositionEmbeddings.
vocabSize := model config at: #vocabSize.

"Model architecture information"
model config keysAndValuesDo: [:key :value |
    Transcript show: key, ': ', value printString; cr
].
```