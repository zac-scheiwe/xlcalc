VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxXl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application
Public WithEvents Wb As Workbook
Public WithEvents Ws As Worksheet
Public Tag As String

Private Sub App_SheetChange(ByVal Sh As Object, ByVal Target As Range)
    XLog.Lg Tag & " app.change " & Sh.Name & "!" & Target.Address(0, 0)
End Sub

Private Sub App_SheetCalculate(ByVal Sh As Object)
    XLog.Lg Tag & " app.calc " & Sh.Name
End Sub

Private Sub App_WorkbookNewSheet(ByVal Wb As Workbook, ByVal Sh As Object)
    XLog.Lg Tag & " app.newsheet " & Sh.Name
End Sub

Private Sub Wb_SheetChange(ByVal Sh As Object, ByVal Target As Range)
    XLog.Lg Tag & " wb.change " & Sh.Name & "!" & Target.Address(0, 0)
End Sub

Private Sub Wb_SheetCalculate(ByVal Sh As Object)
    XLog.Lg Tag & " wb.calc " & Sh.Name
End Sub

Private Sub Wb_NewSheet(ByVal Sh As Object)
    XLog.Lg Tag & " wb.newsheet " & Sh.Name
End Sub

Private Sub Ws_Change(ByVal Target As Range)
    XLog.Lg Tag & " ws.change " & Target.Address(0, 0)
End Sub

Private Sub Ws_Calculate()
    XLog.Lg Tag & " ws.calc"
End Sub

Private Sub Class_Terminate()
    XLog.Lg Tag & " xl term"
End Sub
