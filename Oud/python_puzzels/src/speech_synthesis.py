from gtts import gTTS
import playsound

def text_to_speech(text):
    tts = gTTS(text=text, lang='en')
    filename = "speech.mp3"
    tts.save(filename)
    playsound.playsound(filename, True)

def main():
    text_to_speech("Hello, this is a speech synthesis test.")
    
if __name__ == "__main__":
    main()
