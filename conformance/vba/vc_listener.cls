VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsListener"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Src As ClsEvt
Public Tag As String

Private Sub Src_Fired(ByVal n As Long, ByRef Cancel As Boolean)
    On Error GoTo EH
    VcLog.Lg Tag & " got " & n
    If n > 2 Then Cancel = True
    Exit Sub
EH:
    VcLog.Lg "listener error " & Err.Number
End Sub
