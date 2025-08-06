# EmbeddingLayer Class Documentation

## Class Comment

```smalltalk
"
EmbeddingLayer manages the token and positional embedding components that convert discrete token sequences into continuous vector representations for transformer processing. This layer serves as the input interface to the transformer, combining semantic token embeddings with positional information to create rich input representations.

The embedding layer performs two primary functions: converting token IDs to dense vector representations through a learned vocabulary embedding matrix, and adding positional information through either learned positional embeddings or sinusoidal position encodings. The combination of these embeddings provides the transformer with both semantic and structural information about the input sequence.

Mathematical Foundation:
For input token sequence [t₁, t₂, ..., tₙ] at positions [0, 1, ..., n-1]:
Output[i] = TokenEmbedding[tᵢ] + PositionalEmbedding[i]

Where:
- TokenEmbedding: [vocabularySize × hiddenSize] lookup table for token representations
- PositionalEmbedding: [maxSequenceLength × hiddenSize] position-specific vectors
- hiddenSize: Model's hidden dimension (typically 512, 768, 1024, etc.)

The layer supports both learned positional embeddings (trainable parameters) and fixed sinusoidal encodings (deterministic position-dependent patterns).

Key Responsibilities:
- Convert token IDs to dense vector representations via vocabulary lookup
- Add positional information to enable sequence order understanding
- Manage vocabulary mappings and tokenization integration
- Provide embedding space analysis capabilities for interpretability
- Support embedding interventions and representation patching

Instance Variables:
- vocabulary: Dictionary mapping tokens to IDs and vice versa (bidirectional lookup)
- embedding: Token embedding matrix [vocabularySize × hiddenSize] containing learned token representations
- positional: Positional embedding matrix [maxSequenceLength × hiddenSize] for position encoding
- vocabularySize: Integer representing the total number of tokens in the vocabulary
- maxSequenceLength: Integer representing maximum supported sequence length
- hiddenSize: Integer representing the embedding dimension

The layer inherits index, model, and config from TransformerLayer, though EmbeddingLayer typically has index 0 as it serves as the input layer to the transformer stack.

Usage Patterns:
EmbeddingLayer is typically accessed for vocabulary analysis, embedding space exploration, and input representation studies. Common patterns include analyzing token similarities, exploring semantic clusters in embedding space, and performing embedding-level interventions.

Integration Points:
- Tokenizer: Converts text to token IDs for embedding lookup
- ActivationTensor: Outputs embedded representations for transformer processing
- LinearProbe: Uses embedding representations for semantic analysis
- InterventionHook: Enables embedding-level interventions and patching

Examples:

Basic embedding lookup and analysis:
```smalltalk
| model embeddingLayer tokens embeddings |
model := TransformerModel fromHuggingFace: 'gpt2-small'.
embeddingLayer := model layerAt: 0. \"Embedding layer is typically first\"
tokens := model tokenizer encode: 'The cat sat on the mat'.

\"Get embeddings for token sequence\"
embeddings := embeddingLayer forward: tokens.
embeddings shape. \"Returns #(batchSize seqLen hiddenSize)\"
```

Vocabulary exploration:
```smalltalk
| embeddingLayer vocabulary tokenId embedding |
embeddingLayer := model layerAt: 0.
vocabulary := embeddingLayer vocabulary.

\"Look up token information\"
tokenId := vocabulary idFor: 'cat'.
embedding := embeddingLayer embedding rowAt: tokenId.

\"Find similar tokens by embedding similarity\"
similarTokens := embeddingLayer findSimilarTokens: 'cat' count: 10.
similarTokens do: [:token | 
    Transcript show: token, ' (similarity: ', 
        (embeddingLayer similarity: 'cat' to: token) asString, ')'].
```

Embedding space analysis:
```smalltalk
| embeddingLayer analyzer clusters |
embeddingLayer := model layerAt: 0.
analyzer := EmbeddingAnalyzer for: embeddingLayer.

\"Discover semantic clusters in embedding space\"
clusters := analyzer findSemanticClusters: 50. \"Find 50 clusters\"
clusters do: [:cluster |
    Transcript show: 'Cluster theme: ', cluster theme.
    Transcript show: 'Representative tokens: ', cluster tokens asString].
```

Positional embedding analysis:
```smallttml
| embeddingLayer positionalMatrix positionSimilarity |
embeddingLayer := model layerAt: 0.
positionalMatrix := embeddingLayer positional.

\"Analyze how positional embeddings change across sequence\"
positionSimilarity := Array new: 100.
1 to: 100 do: [:i |
    positionSimilarity at: i put: 
        (positionalMatrix cosineDistance: (positionalMatrix rowAt: 1) 
                                     to: (positionalMatrix rowAt: i))].

\"Plot similarity decay with distance\"
positionSimilarity plot: 'Position Similarity Decay'.
```

Embedding intervention experiments:
```smalltalk
| embeddingLayer interventionHook |
embeddingLayer := model layerAt: 0.

\"Replace 'cat' embeddings with 'dog' embeddings\"
interventionHook := InterventionHook new
    name: 'cat-to-dog-substitution';
    condition: [:layer | layer == embeddingLayer];
    action: [:activation |
        | catId dogId catEmbedding dogEmbedding |
        catId := embeddingLayer vocabulary idFor: 'cat'.
        dogId := embeddingLayer vocabulary idFor: 'dog'.
        dogEmbedding := embeddingLayer embedding rowAt: dogId.
        activation replaceTokenEmbedding: catId with: dogEmbedding.
        activation];
    yourself.
        
embeddingLayer addHook: interventionHook.
```

Token frequency and embedding quality analysis:
```smalltalk
| embeddingLayer tokenStats qualityMetrics |
embeddingLayer := model layerAt: 0.

\"Analyze relationship between token frequency and embedding quality\"
tokenStats := embeddingLayer analyzeTokenStatistics: trainingCorpus.
qualityMetrics := embeddingLayer computeEmbeddingQuality.

\"Find undertrained tokens (low frequency, poor embedding quality)\"
undertrainedTokens := tokenStats select: [:stat |
    (stat frequency < 100) and: [stat embeddingQuality < 0.5]].
    
undertrainedTokens do: [:token |
    Transcript show: 'Undertrained: ', token text, 
                   ' (freq: ', token frequency asString, 
                   ', quality: ', token embeddingQuality asString, ')'].
```

Embedding space visualization:
```smalltalk
| embeddingLayer visualizer |
embeddingLayer := model layerAt: 0.
visualizer := EmbeddingVisualizer for: embeddingLayer.

\"Create interactive 2D projection of embedding space\"
visualizer
    projectUsing: #tSNE; \"or #PCA, #UMAP\"
    highlightTokens: #('cat' 'dog' 'animal' 'pet');
    colorByCategory: #partOfSpeech;
    openInBrowser.
```

Subword tokenization analysis:
```smalltalk
| embeddingLayer tokenizer subwordAnalysis |
embeddingLayer := model layerAt: 0.
tokenizer := model tokenizer.

\"Analyze how subword tokenization affects embeddings\"
subwordAnalysis := embeddingLayer analyzeSubwordComposition: 'unhappiness'.
subwordAnalysis do: [:component |
    Transcript show: 'Subword: ', component token, 
                   ' contributes: ', component contribution asString].
```
"
```

## Implementation Notes

The EmbeddingLayer implementation optimizes for both lookup efficiency and interpretability access. The vocabulary is implemented as a bidirectional hash table enabling fast token-to-ID and ID-to-token lookups.

Positional embeddings support both learned and fixed (sinusoidal) encodings, with automatic detection based on model configuration. The layer handles variable sequence lengths efficiently by only computing positional embeddings for the actual sequence length.

Embedding interventions are implemented through efficient tensor operations that avoid full matrix copying, enabling real-time experimentation with embedding modifications.

## Mathematical Details

The embedding layer implements several position encoding strategies:

### Learned Positional Embeddings
Standard trainable parameters that learn position-specific representations during training. Each position has an independent embedding vector.

### Sinusoidal Position Encoding
Fixed encodings using sine and cosine functions of different frequencies:
PE(pos, 2i) = sin(pos / 10000^(2i/hiddenSize))
PE(pos, 2i+1) = cos(pos / 10000^(2i/hiddenSize))

This encoding allows the model to extrapolate to sequence lengths longer than those seen during training.

### Relative Position Encoding
Some variants support relative position encoding where position information is computed relative to other tokens rather than absolute positions.

## Vocabulary Management

The vocabulary system supports various tokenization schemes:
- **Word-level**: Each word is a separate token
- **Subword-level**: Words are split into subword units (BPE, WordPiece)
- **Character-level**: Individual characters as tokens

The layer provides utilities for analyzing tokenization quality and identifying problematic token splits.

## Related Classes

- **TransformerLayer**: Abstract base class providing common layer interface
- **Tokenizer**: Converts text to token sequences for embedding lookup
- **EmbeddingAnalyzer**: Specialized analysis tools for embedding space exploration
- **EmbeddingVisualizer**: Interactive visualization of embedding spaces
- **InterventionHook**: Enables embedding-level interventions and patching
- **LinearProbe**: Uses embedding representations for semantic analysis tasks