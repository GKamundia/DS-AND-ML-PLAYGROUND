from fastapi import FastAPI, File, UploadFile
from fastapi.middleware.cors import CORSMiddleware
import uvicorn
import numpy as np
from io import BytesIO
from PIL import Image
import tensorflow as tf

app = FastAPI()

origins = [
    "http://localhost",
    "http://localhost:3000",
]
app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Assuming you have IMG_HEIGHT, IMG_WIDTH, IMG_CHANNELS defined
IMG_HEIGHT, IMG_WIDTH, IMG_CHANNELS = 224, 224, 3

MODEL = tf.keras.models.load_model("C:/Users/Anarchy/Documents/Data_Science/1", compile=False)
MODEL.build((None, IMG_HEIGHT, IMG_WIDTH, IMG_CHANNELS))

CLASS_NAMES = ["Sick", "Healthy"]

@app.get("/ping")
async def ping():
    return "Hello, I am alive"

def read_file_as_image(data) -> np.ndarray:
    image = Image.open(BytesIO(data))
    # Resize the image to (224, 224) and add the color channel
    image = image.resize((224, 224))
    image = np.array(image)
    
    # If the image is grayscale, convert it to RGB
    if len(image.shape) == 2:
        image = np.stack([image] * 3, axis=-1)

    return image

@app.post("/predict")
async def predict(
    file: UploadFile = File(...)
):
    image = read_file_as_image(await file.read())
    image = image / 255.0  # Normalize the pixel values if needed
    
    # Convert NumPy array to TensorFlow tensor
    img_batch = tf.convert_to_tensor(np.expand_dims(image, axis=0), dtype=tf.float32)

    predictions = MODEL.predict(img_batch)

    # Get the class label and confidence level
    class_label = CLASS_NAMES[0] if predictions[0] > 0.5 else CLASS_NAMES[1]
    confidence = float(np.max(predictions[0]))

    # Invert confidence if class_label is associated with CLASS_NAMES[1]
    confidence = confidence if class_label == CLASS_NAMES[0] else 1 - confidence
    

    return {
        'class': class_label,
        'confidence': confidence
    }

if __name__ == "__main__":
    uvicorn.run(app, host='localhost', port=8000)

