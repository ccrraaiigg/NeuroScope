loadWeightsFromHuggingFace: modelName intoLayers: layers
	"Downloads and loads pre-trained weights from HuggingFace Hub into the transformer layers.
	
	Parameters:
	- modelName (String): The HuggingFace model identifier
	- layers (OrderedCollection): Collection of TransformerLayer instances to load weights into"
	
	| weightsUrl weightsData |
	
	"Construct URL for model weights (pytorch_model.bin or model.safetensors)"
	weightsUrl := self constructWeightsUrl: modelName.
	
	"Download weights file"
	[
		weightsData := JSInterface downloadBinaryFile: weightsUrl.
		weightsData ifNil: [
			NetworkError signal: 'Failed to download model weights from: ', weightsUrl
		].
		
		"Parse and load weights into layers"
		self parseAndLoadWeights: weightsData intoLayers: layers.
		
	] on: Error do: [:ex |
		NetworkError signal: 'Error loading model weights: ', ex messageText
	]