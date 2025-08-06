# tokenizer

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