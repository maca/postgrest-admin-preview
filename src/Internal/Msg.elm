module Internal.Msg exposing (Msg(..))

import Browser
import Internal.Application exposing (Params)
import Internal.Client as Client
import Internal.Cmd as AppCmd
import Internal.Notification as Notification
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing
import PostgrestAdmin.Client as Client exposing (Client)
import Url exposing (Url)
import Utils.Task exposing (Error(..))


type Msg m msg
    = ApplicationInit ( Params m msg, m ) (List (AppCmd.Cmd msg))
    | ClientChanged Client.Msg
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageApplicationChanged msg
    | RequestPerformed Client (Msg m msg)
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOp
