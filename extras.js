
function loadExtras(app) {
  const oauthClientIdKey = 'oauthClientId';
  app.ports.setOAuthClientId.subscribe(id => {
    localStorage.setItem(oauthClientIdKey, id);
  });
  app.ports.requestOAuthClientId.subscribe(() => {
    app.ports.receiveOAuthClientId.send(
      localStorage.getItem(oauthClientIdKey));
  });

  const oauthAccessTokenKey = 'oauthAccessToken';
  app.ports.setOAuthAccessToken.subscribe(token => {
    localStorage.setItem(oauthAccessTokenKey, token);
  });
  app.ports.requestOAuthAccessToken.subscribe(() => {
    app.ports.receiveOAuthAccessToken.send(
      localStorage.getItem(oauthAccessTokenKey));
  });

}
