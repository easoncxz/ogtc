
function loadExtras(app) {
  const oauthClientIdKey = 'oauthClientId';
  app.ports.setOAuthClientId.subscribe(id => {
    localStorage.setItem(oauthClientIdKey, id);
  });
  app.ports.requestOAuthClientId.subscribe(() => {
    app.ports.receiveOAuthClientId.send(
      localStorage.getItem(oauthClientIdKey));
  });
}
