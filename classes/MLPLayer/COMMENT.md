# MLPLayer Class Documentation

## Class Comment

```smalltalk
"
MLPLayer implements the feed-forward network component of transformer architectures, providing the non-linear transformation that complements the attention mechanism. This layer consists of two linear transformations with a non-linear activation function in between, typically expanding the hidden dimension by a factor of 4 before projecting back to the original size.

The MLP (Multi-Layer Perceptron) serves as the 'thinking' component of each transformer layer, where the model processes and transforms the representations gathered by the attention mechanism. The expansion and contraction pattern allows the model to compute complex non-linear functions over the input representations.

Mathematical Foundation:
MLP(x) = downWeights × activation(upWeights × x + upBias) + downBias

Where:
- upWeights: [hiddenSize × expandedSize] - expands representation to intermediate dimension
- downWeights: [expandedSize × hiddenSize] - projects back to original dimension  
- activation: Non-linear function (typically GELU, ReLU, or SwiGLU)
- expandedSize: Usually 4 × hiddenSize (configurable expansion factor)

The two-layer structure with intermediate expansion enables the model to learn complex non-linear mappings while maintaining computational efficiency through the bottleneck architecture.

Key Responsibilities:
- Execute feed-forward computation with configurable activation functions
- Manage weight matrices for expansion and contraction transformations
- Provide neuron-level access for interpretability analysis
- Support activation patching and intervention experiments
- Enable efficient weight matrix analysis and visualization

Instance Variables:
- upWeights: Weight matrix for expanding hidden representations [hiddenSize × expandedSize]
- downWeights: Weight matrix for contracting to original dimension [expandedSize × hiddenSize]
- activation: Activation function object (GELU, ReLU, SwiGLU, etc.)
- upBias: Optional bias vector for expansion transformation [expandedSize]
- downBias: Optional bias vector for contraction transformation [hiddenSize]
- expandedSize: Integer representing intermediate dimension (typically 4 × hiddenSize)

The layer inherits index, model, and config from TransformerLayer, providing access to layer position, parent model, and configuration parameters including activation function type and expansion factor.

Usage Patterns:
MLPLayer instances are accessed through the model's layer collection for neuron-level analysis, weight inspection, and intervention experiments. Common patterns include identifying important neurons, analyzing weight patterns, and performing activation patching to understand the layer's computational role.

Integration Points:
- ActivationTensor: Processes MLP inputs and outputs with shape [batchSize, seqLen, hiddenSize]
- NeuronAnalyzer: Analyzes individual neuron behaviors and activations
- InterventionHook: Enables neuron-level interventions and activation patching
- LinearProbe: Uses MLP activations for representation analysis

Examples:

Basic MLP computation and analysis:
```smalltalk
| model mlpLayer tokens activations |
model := TransformerModel fromHuggingFace: 'gpt2-small'.
mlpLayer := model layerAt: 8.
tokens := model tokenizer encode: 'The cat sat on the mat'.

\"Get MLP activations for analysis\"
activations := mlpLayer forward: (model activationsAt: 8 for: tokens).
activations shape. \"Returns #(batchSize seqLen hiddenSize)\"
```

Neuron-level analysis:
```smalltalk
| mlpLayer analyzer topNeurons |
mlpLayer := model layerAt: 5.
analyzer := NeuronAnalyzer for: mlpLayer.

\"Find neurons that activate strongly on specific patterns\"
topNeurons := analyzer findTopActivatingNeurons: 'programming languages'.
topNeurons do: [:neuron |
    Transcript show: 'Neuron ', neuron index asString, 
                   ' activates on: ', neuron topTokens asString].
```

Weight matrix analysis:
```smalltalk
| mlpLayer upMatrix downMatrix neuronWeights |
mlpLayer := model layerAt: 3.

\"Access weight matrices\"
upMatrix := mlpLayer upWeights. \"Expansion weights\"
downMatrix := mlpLayer downWeights. \"Contraction weights\"

\"Analyze specific neuron's input and output connections\"
neuronWeights := Dictionary new.
neuronWeights at: 'input' put: (upMatrix columnAt: 1024). \"Neuron 1024's inputs\"
neuronWeights at: 'output' put: (downMatrix rowAt: 1024). \"Neuron 1024's outputs\"
```

Activation patching experiment:
```smallttml
| mlpLayer patchHook cleanActivations |
mlpLayer := model layerAt: 6.

\"Store clean activations for patching\"
cleanActivations := mlpLayer forward: (model activationsAt: 6 for: cleanTokens).

\"Create intervention to patch specific neurons\"
patchHook := InterventionHook new
    name: 'patch-neurons-100-200';
    condition: [:layer | layer == mlpLayer];
    action: [:activation |
        \"Patch neurons 100-200 with clean activations\"
        activation patchNeurons: (100 to: 200) with: cleanActivations.
        activation];
    yourself.
        
mlpLayer addHook: patchHook.
```

Activation function analysis:
```smallttml
| mlpLayer activationFn intermediateValues |
mlpLayer := model layerAt: 4.
activationFn := mlpLayer activation.

\"Analyze activation function behavior\"
intermediateValues := mlpLayer upWeights * inputActivations.
activatedValues := activationFn apply: intermediateValues.

\"Compare pre and post activation patterns\"
Transcript show: 'Sparsity before activation: ', 
    (intermediateValues sparsityRatio asString).
Transcript show: 'Sparsity after activation: ', 
    (activatedValues sparsityRatio asString).
```

Neuron importance ranking:
```smalltalk
| mlpLayer importanceScores topNeurons |
mlpLayer := model layerAt: 7.

\"Compute neuron importance based on activation magnitude\"
importanceScores := mlpLayer computeNeuronImportance: testDataset.
topNeurons := importanceScores sortedByValue reverse first: 50.

\"Analyze top neurons\"
topNeurons do: [:assoc |
    | neuronIndex importance |
    neuronIndex := assoc key.
    importance := assoc value.
    Transcript show: 'Neuron ', neuronIndex asString, 
                   ' importance: ', importance asString].
```
"
```

## Implementation Notes

The MLPLayer implementation optimizes for both computational efficiency and interpretability access. Weight matrices are stored in formats that enable efficient GPU computation while providing easy access for analysis tools.

The layer supports various activation functions through a pluggable architecture, allowing researchers to experiment with different non-linearities. Activation functions are implemented as first-class objects with both forward and derivative computation capabilities.

Neuron-level interventions are implemented efficiently by maintaining sparse representations of modifications, avoiding full tensor copying when only specific neurons are being manipulated.

## Mathematical Details

The feed-forward network follows the standard transformer MLP formulation with several implementation optimizations:

1. **Expansion Phase**: Linear transformation expands hidden dimension by expansion factor (typically 4x)
2. **Activation**: Non-linear function applied element-wise to expanded representation
3. **Contraction Phase**: Linear transformation projects back to original hidden dimension
4. **Residual Connection**: Output is added to layer input (handled by parent TransformerLayer)

The layer supports various activation functions:
- **GELU**: Gaussian Error Linear Unit (most common in modern transformers)
- **ReLU**: Rectified Linear Unit (simpler, more interpretable)
- **SwiGLU**: Swish-Gated Linear Unit (used in some recent architectures)

Bias terms are optional and configurable based on model architecture requirements.

## Performance Considerations

The MLP layer typically represents the computational bottleneck in transformer models due to the large matrix multiplications involved. The implementation uses several optimization strategies:

- **WebGL Acceleration**: Matrix operations are performed on GPU when available
- **Batch Processing**: Multiple sequences processed simultaneously for efficiency
- **Memory Layout**: Optimized tensor layouts minimize data movement
- **Sparse Interventions**: Neuron-level modifications use sparse representations

## Related Classes

- **TransformerLayer**: Abstract base class providing common layer interface
- **NeuronAnalyzer**: Specialized analysis tools for individual neuron behaviors
- **InterventionHook**: Enables neuron-level interventions and activation patching
- **LinearProbe**: Uses MLP representations for downstream analysis tasks
- **ActivationTensor**: Manages MLP computation inputs and outputs