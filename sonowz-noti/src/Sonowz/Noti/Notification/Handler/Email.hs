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
  { emailConfigEmail :: Address,
    emailConfigPassword :: Text,
    emailConfigHostname :: Text,
    emailConfigPort :: Int
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
      (toString emailConfigHostname)
      (fromIntegral emailConfigPort)
      (toString $ addressEmail emailConfigEmail)
      (toString emailConfigPassword)
      (makeMail config noti)

makeMail :: EmailConfig -> Notification -> Mail
makeMail EmailConfig {..} Notification {..} = simpleMail from to cc bcc title [body]
  where
    from = emailConfigEmail
    to = [emailConfigEmail]
    cc = []
    bcc = []
    title = notificationTitle
    body = case notificationBody of
      HTMLBody html -> htmlPart $ fromStrict html
      TextBody text -> plainPart $ fromStrict text
