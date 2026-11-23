VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "HlApp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application

Private Sub App_SheetFollowHyperlink(ByVal Sh As Object, ByVal Target As Hyperlink)
    HlLog.L "App.SheetFollow " & Sh.Name & " " & HlLog.Desc(Target) & " wb=" & ActiveWorkbook.Name & " active=" & ActiveSheet.Name & " sel=" & Selection.Address
End Sub
