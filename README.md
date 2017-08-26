# HomedManager

HomedManager is a simple tool to manage your [HiDrive](https://www.free-hidrive.com) account. 

## Get started
Create a homedmanager.yaml in your HOME directory.
The content of this file:

```
client_id: "CLIENT_ID"
client_secret: "CLIENT_SECRET"
authCode: "AUTH_CODE"
authTokenUrl: "https://www.hidrive.strato.com/oauth2/token"
authTokenInfoUrl: "https://www.hidrive.strato.com/oauth2/tokeninfo"
apiUrl: "https://api.hidrive.strato.com"
```

Get CLIENT_ID and CLIENT_SECRET: [GET API KEY](https://dev.strato.com/hidrive/get_key)

AUTH_CODE is the code obtained when you authorize this app
```
https://www.hidrive.strato.com/oauth2/authorize?client_id=CLIENT_ID&response_type=code&scope=admin,rw
```
