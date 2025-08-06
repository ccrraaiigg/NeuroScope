# generate: prompt maxTokens: maxTokens

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