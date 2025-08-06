# fromHuggingFace: modelName

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