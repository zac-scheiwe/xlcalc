Attribute VB_Name = "CdTag"
Option Explicit

Public CdS As String

Public Function CdT(ByVal tag As String) As Variant
    On Error GoTo EH
    CdS = CdS & tag & "|"
    CdT = 0
    Exit Function
EH:
    CdT = -1
End Function

Public Function GetCdLog() As String
    On Error GoTo EH
    GetCdLog = CdS
    CdS = ""
    Exit Function
EH:
    GetCdLog = "GetCdLog error " & Err.Number
End Function
