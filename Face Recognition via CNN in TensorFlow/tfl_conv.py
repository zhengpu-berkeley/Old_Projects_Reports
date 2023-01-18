import tensorflow as tf

def conv():
    #converting keras model into tensorflow lite model
    model = tf.keras.models.load_model('model_trained/model_epoch_vgg19_25.h5')
    model_conv = tf.lite.TFLiteConverter.from_keras_model(model)
    model_tflite = model_conv.convert()
    open('model_tflite/model_epoch_vgg19_25.tflite','wb').write(model_tflite)