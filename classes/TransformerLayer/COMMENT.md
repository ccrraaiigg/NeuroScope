# TransformerLayer Class Documentation

## Class Comment

```smalltalk
"
TransformerLayer serves as the abstract base class for all transformer neural network layer types in the NeuroScope framework. This class defines the common interface and contracts that all concrete layer implementations must follow, providing a unified approach to layer management, parameter access, and hook integration.

The TransformerLayer abstraction enables polymorphic treatment of different layer types (attention, MLP, embedding) while maintaining type safety and consistent behavior across the transformer architecture. Each layer maintains references to its position in the model, the parent model instance, and configuration parameters that govern its behavior.

Key Responsibilities:
- Define common interface for all transformer layer types
- Manage layer position and model relationships
- Provide standardized parameter access patterns
- Enable hook integration for interventions and analysis
- Maintain configuration state and layer metadata

Instance Variables:
- index: Integer representing the layer's position in the transformer stack (0-based indexing)
- model: Reference to the parent TransformerModel instance that contains this layer
- config: Dictionary containing layer-specific configuration parameters and hyperparameters

The layer hierarchy follows a clear contract where each concrete subclass implements the forward: method for computation, while this base class provides common functionality for parameter access, hook management, and model integration.

Usage Patterns:
Layers are typically accessed through the model's layer collection rather than instantiated directly. The common pattern involves retrieving layers by index and then accessing their parameters or attaching hooks for analysis.

Integration Points:
- TransformerModel: Parent model that manages the layer collection
- HookManager: Enables intervention and analysis hook attachment
- ActivationTensor: Processes layer inputs and outputs
- Configuration system: Manages layer hyperparameters and settings

Examples:

Basic layer access and parameter inspection:
```smalltalk
| model layer |
model := TransformerModel fromHuggingFace: 'gpt2-small'.
layer := model layerAt: 5.
layer index. \"Returns 5\"
layer config at: 'hiddenSize'. \"Returns layer's hidden dimension\"
```

Hook integration for analysis:
```smalltalk
| layer hook |
layer := model layerAt: 3.
hook := ActivationHook new
    name: 'layer3-analysis';
    action: [:activation | activation inspect];
    yourself.
layer addHook: hook.
```

Parameter access patterns:
```smalltalk
| layer weights |
layer := model layerAt: 0.
weights := layer parameters. \"Returns dictionary of all layer parameters\"
layer hasParameter: 'bias'. \"Check if layer has bias parameters\"
```
"
```

## Implementation Notes

The TransformerLayer class follows Smalltalk's abstract class pattern, where the base class defines the interface but concrete implementations are required for actual computation. The class leverages Smalltalk's dynamic typing while maintaining clear contracts through method signatures and documentation.

The layer indexing system uses 0-based indexing to align with common ML framework conventions, making it easier to correlate with external model definitions and research papers.

## Related Classes

- **AttentionLayer**: Concrete implementation for multi-head attention mechanisms
- **MLPLayer**: Concrete implementation for feed-forward network layers  
- **EmbeddingLayer**: Concrete implementation for token and positional embeddings
- **TransformerModel**: Parent model class that manages layer collections
- **HookManager**: Manages intervention and analysis hooks attached to layers