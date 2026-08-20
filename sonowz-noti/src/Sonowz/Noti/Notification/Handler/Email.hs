module Sonowz.Noti.Notification.Handler.Email
  ( EmailConfig (..),
    generateEmailNotification,
  )
where

import Network.Mail.Mime (Address, Mail, addressEmail, htmlPart, plainPart)
import Network.Mail.SMTP (sendMailWithLoginSTARTTLS', simpleMail)
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types

data EmailConfig = EmailConfig
  { email :: Address,
    password :: Text,
    hostname :: Text,
    port :: Int
  }

generateEmailNotification ::
  forall r.
  Members '[Reader EmailConfig, Embed IO] r =>
  Notification ->
  Sem r ()
generateEmailNotification noti = do
  config@EmailConfig {..} <- ask
  liftIO $
    sendMailWithLoginSTARTTLS'
      (toString hostname)
      (fromIntegral port)
      (toString $ addressEmail email)
      (toString password)
      (makeMail config noti)

makeMail :: EmailConfig -> Notification -> Mail
makeMail EmailConfig {..} Notification {..} = simpleMail from to cc bcc title [bodyPart]
  where
    from = email
    to = [email]
    cc = []
    bcc = []
    bodyPart = case body of
      HTMLBody html -> htmlPart $ fromStrict html
      TextBody text -> plainPart $ fromStrict text
