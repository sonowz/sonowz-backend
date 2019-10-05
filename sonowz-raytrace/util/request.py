import websocket
try:
    import thread
except ImportError:
    import _thread as thread
import time

def on_message(ws, message):
    print(message)

def on_error(ws, error):
    print(error)

def on_close(ws):
    print("### closed ###")

def on_open(ws):
    def run(*args):
        ws.send('\
	{ "pixelWidth"   : 300\
        , "pixelHeight"  : 300\
        , "jittering"    : false\
        , "areaLight"    : false\
        , "antiAliasing" : true\
        , "dofToggle"    : false\
        , "dofAperture"  : 5.0\
        , "dofFocus"     : 400\
        , "sceneNo"      : 2\
        }')
        print("message sent.")
    thread.start_new_thread(run, ())


if __name__ == "__main__":
    websocket.enableTrace(True)
    ws = websocket.WebSocketApp("ws://localhost:3333/raytrace/wait/",
                              on_message = on_message,
                              on_error = on_error,
                              on_close = on_close)
    ws.on_open = on_open
    ws.run_forever()

