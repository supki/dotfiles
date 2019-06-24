#NoEnv
#InstallKeybdHook
SendMode Input
SetWorkingDir %A_ScriptDir%

; F1 key by default, Fn+F1 is now Mute/Unmute
VOLUME_MUTE::F1
F1::VOLUME_MUTE

; F2 key by default, Fn+F2 is now Volume Down
VOLUME_DOWN::F2
F2::VOLUME_DOWN

; F3 key by default, Fn+F3 is now Volume Up
VOLUME_UP::F3
F3::VOLUME_UP

; F4 key by default, Fn+F4 is now Media Prev
MEDIA_PREV::F4
F4::MEDIA_PREV

; F5 key by default, Fn+F5 is now Play/Pause
MEDIA_PLAY_PAUSE::F5
F5::MEDIA_PLAY_PAUSE

; F6 key by default, Fn+F6 is now Media Next
MEDIA_NEXT::F6
F6::MEDIA_NEXT

; F6-F7 are fucked by not being actual keys
; F8-F12 are weird combinations of modifiers and F21, can't be bothered with
; making that work flawlessly together with the most important thing in life:

; not being a peasant
+CapsLock::CapsLock
CapsLock::LCtrl

; It would've been great to be able to CapsLockify LCtrl instead of Shift+CapsLock
; but that makes Chrome to freak out for no reason at all. An amazing combo of shit
; hardware and inane software.
