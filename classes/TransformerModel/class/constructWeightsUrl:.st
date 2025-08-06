constructWeightsUrl: modelName
	"Constructs the appropriate URL for downloading model weights from HuggingFace Hub.
	Tries safetensors format first, falls back to pytorch_model.bin.
	
	Parameters:
	- modelName (String): The HuggingFace model identifier
	
	Returns: String containing the weights file URL"
	
	| baseUrl safetensorsUrl pytorchUrl |
	
	baseUrl := 'https://huggingface.co/', modelName, '/resolve/main/'.
	safetensorsUrl := baseUrl, 'model.safetensors'.
	pytorchUrl := baseUrl, 'pytorch_model.bin'.
	
	"Check if safetensors format is available (preferred)"
	(JSInterface checkUrlExists: safetensorsUrl) ifTrue: [
		^safetensorsUrl
	].
	
	"Fall back to PyTorch format"
	(JSInterface checkUrlExists: pytorchUrl) ifTrue: [
		^pytorchUrl
	].
	
	"Neither format found"
	NetworkError signal: 'No compatible weights file found for model: ', modelName