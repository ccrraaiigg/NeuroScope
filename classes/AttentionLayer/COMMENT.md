# AttentionLayer Class Documentation

## Class Comment

```smalltalk
"
AttentionLayer implements the multi-head attention mechanism that forms the core of transformer architectures. This layer performs the scaled dot-product attention computation across multiple attention heads, enabling the model to focus on different aspects of the input sequence simultaneously.

The attention mechanism computes attention weights by taking the dot product of queries and keys, scaling by the square root of the key dimension, applying softmax normalization, and then using these weights to compute a weighted sum of values. Multiple attention heads allow the model to attend to information from different representation subspaces at different positions.

Mathematical Foundation:
For each attention head h, the computation follows:
Attention(Q, K, V) = softmax(QK^T / √d_k)V

Where:
- Q (queries) = input × queryWeights[h]
- K (keys) = input × keyWeights[h]  
- V (values) = input × valueWeights[h]
- d_k = dimension of key vectors (headDim)

The outputs from all heads are concatenated and projected through outputWeights to produce the final layer output.

Key Responsibilities:
- Execute multi-head attention computation with configurable head count
- Manage separate weight matrices for queries, keys, values, and output projection
- Provide access to attention patterns for interpretability analysis
- Support head-specific interventions and activation patching
- Enable efficient attention pattern extraction and visualization

Instance Variables:
- headCount: Integer specifying the number of attention heads (typically 8, 12, or 16)
- queryWeights: Array of weight matrices for computing queries, one per head [headCount × (hiddenSize × headDim)]
- keyWeights: Array of weight matrices for computing keys, one per head [headCount × (hiddenSize × headDim)]
- valueWeights: Array of weight matrices for computing values, one per head [headCount × (hiddenSize × headDim)]
- outputWeights: Weight matrix for final output projection [hiddenSize × hiddenSize]
- headDim: Integer representing the dimension of each attention head (hiddenSize / headCount)

The layer inherits index, model, and config from TransformerLayer, providing access to layer position, parent model, and configuration parameters including dropout rates and initialization schemes.

Usage Patterns:
AttentionLayer instances are typically accessed through the model's layer collection for analysis and intervention. Common patterns include extracting attention patterns, performing head-specific interventions, and analyzing the contribution of individual attention heads to model behavior.

Integration Points:
- ActivationTensor: Processes attention inputs and outputs with shape [batchSize, seqLen, hiddenSize]
- AttentionAnalyzer: Analyzes attention patterns and head behaviors
- InterventionHook: Enables attention head zeroing and activation patching
- AttentionVisualizer: Renders attention patterns for interactive exploration

Examples:

Basic attention pattern extraction:
```smalltalk
| model attentionLayer tokens attentionPatterns |
model := TransformerModel fromHuggingFace: 'gpt2-small'.
attentionLayer := model layerAt: 6.
tokens := model tokenizer encode: 'The cat sat on the mat'.

\"Extract attention patterns for all heads\"
attentionPatterns := attentionLayer extractAttentionPatterns: tokens.
attentionPatterns at: 1. \"Attention pattern for head 0 [seqLen × seqLen]\"
```

Head-specific intervention:
```smalltalk
| attentionLayer interventionHook |
attentionLayer := model layerAt: 8.

\"Zero out attention head 3 to study its contribution\"
interventionHook := InterventionHook new
    name: 'zero-head-3';
    condition: [:layer | layer == attentionLayer];
    action: [:activation | 
        activation zeroAttentionHead: 3.
        activation];
    yourself.
        
attentionLayer addHook: interventionHook.
```

Attention head analysis:
```smalltalk
| attentionLayer analyzer headBehaviors |
attentionLayer := model layerAt: 4.
analyzer := AttentionAnalyzer for: attentionLayer.

\"Analyze what each attention head focuses on\"
headBehaviors := analyzer analyzeHeadBehaviors: 'The quick brown fox jumps'.
headBehaviors do: [:behavior | 
    Transcript show: 'Head ', behavior headIndex asString, 
                   ' focuses on: ', behavior primaryPattern].
```

Weight matrix access for research:
```smalltalk
| attentionLayer queryMatrix headWeights |
attentionLayer := model layerAt: 2.

\"Access query weights for head 5\"
queryMatrix := attentionLayer queryWeights at: 6. \"1-based indexing\"
queryMatrix shape. \"Returns #(hiddenSize headDim)\"

\"Analyze weight patterns across all heads\"
headWeights := attentionLayer queryWeights collect: [:weights | 
    weights norm]. \"L2 norm of each head's query weights\"
```

Attention pattern visualization:
```smalltalk
| attentionLayer visualizer |
attentionLayer := model layerAt: 7.
visualizer := AttentionVisualizer for: attentionLayer.

\"Create interactive attention pattern display\"
visualizer 
    showText: 'Attention is all you need';
    highlightHeads: #(2 5 8);
    openInBrowser.
```
"
```

## Implementation Notes

The AttentionLayer implementation leverages WebGL acceleration for efficient matrix operations, particularly important for the large matrix multiplications involved in attention computation. The layer supports both training and inference modes, with appropriate handling of dropout and gradient computation.

Attention patterns are cached when extracted to avoid recomputation during analysis workflows. The layer provides both raw attention weights and normalized patterns suitable for visualization.

The multi-head architecture allows for parallel computation of attention heads, with efficient memory layout to minimize data movement between JavaScript and WebGL contexts.

## Mathematical Details

The scaled dot-product attention mechanism implemented follows the standard transformer formulation:

1. **Linear Projections**: Input embeddings are projected to query, key, and value representations
2. **Attention Score Computation**: Dot product of queries and keys, scaled by √d_k
3. **Softmax Normalization**: Attention scores are normalized to form probability distributions
4. **Value Aggregation**: Weighted sum of values using attention probabilities
5. **Output Projection**: Concatenated head outputs are projected to final representation

The layer handles variable sequence lengths efficiently and supports both causal (decoder) and bidirectional (encoder) attention patterns based on configuration.

## Related Classes

- **TransformerLayer**: Abstract base class providing common layer interface
- **AttentionAnalyzer**: Specialized analysis tools for attention patterns
- **AttentionVisualizer**: Interactive visualization of attention mechanisms
- **InterventionHook**: Enables attention head interventions and patching
- **ActivationTensor**: Manages attention computation inputs and outputs