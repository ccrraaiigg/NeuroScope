# forward: tokens

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