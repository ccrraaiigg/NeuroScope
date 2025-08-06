# config

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