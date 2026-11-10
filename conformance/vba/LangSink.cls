VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "LangSink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Src As LangCls
Public Veto As Boolean

Private Sub Src_Changed(ByVal n As Long, Cancel As Boolean)
    L2Log = L2Log & "<ch" & n & ">"
    Cancel = Veto
End Sub

Private Sub Src_Plain()
    L2Log = L2Log & "<pl>"
End Sub
