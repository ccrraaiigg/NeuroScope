# runWithCaching: tokens layers: layerIndices components: componentNames

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