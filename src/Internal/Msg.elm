module Internal.Msg exposing (Msg(..))

import Browser
import Internal.Client as Client
import Internal.Notification as Notification
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing
import PostgrestAdmin.Client as Client exposing (Client)
import Url exposing (Url)
import Utils.Task exposing (Error(..))


type Msg msg
    = ClientChanged Client.Msg
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageApplicationChanged msg
    | RequestPerformed Client (Msg msg)
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error
