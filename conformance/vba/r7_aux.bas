Attribute VB_Name = "VxAux"
Option Explicit

Public Const PubC As Long = 11
Private Const PrivC As Long = 22
Public PubV As Long
Private PrivV As Long

Public Function PubF() As String
    PubF = "pub" & PrivC & PrivV
End Function

Private Function PrivF() As String
    PrivF = "priv"
End Function

Public Function ViaPriv() As String
    ViaPriv = PrivF()
End Function

Public Sub Nop()
End Sub

Public Function Counter() As Long
    PubV = PubV + 1
    PrivV = PrivV + 10
    Counter = PubV + PrivV
End Function
