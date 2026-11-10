use xlcalc_variant::Variant;

use crate::app::{Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};
use crate::window::palette_bgr;

fn international() -> Vec<Variant> {
    use Variant::{Bool, Bstr, R8};
    let mut v = vec![R8(1.0), R8(64.0)];
    for s in [
        ".", ",", ",", "R", "C", "r", "c", "[", "]", "{", "}", ",", ";", "@", "/", ":", "y", "m",
        "d", "h", "m", "s", "$", "General",
    ] {
        v.push(Bstr(s.into()));
    }
    v.extend([2.0, 1.0, 2.0, 3.0, 3.0, 1.0].map(R8));
    v.extend(
        [
            false, false, true, false, true, true, true, true, true, false, true, false, false,
        ]
        .map(Bool),
    );
    v.push(Bstr("en-US".into()));
    v
}

fn default_of(scope: &str, name: &str) -> Option<Variant> {
    use Variant::{Bool, Bstr, I4, R8};
    Some(match (scope, name) {
        (
            "app",
            "EditDirectlyInCell" | "MoveAfterReturn" | "DisplayFormulaBar" | "DisplayStatusBar",
        )
        | ("app", "DisplayScrollBars" | "AlertBeforeOverwriting" | "CellDragAndDrop")
        | ("app", "EnableAutoComplete" | "ExtendList" | "PrintCommunication")
        | ("app", "ShowChartTipNames" | "ShowChartTipValues" | "UseSystemSeparators") => Bool(true),
        ("app", "FixedDecimal" | "IgnoreRemoteRequests" | "RollZoom" | "Interactive") => {
            Bool(false)
        }
        ("app", "CalculationInterruptKey") => I4(2),
        ("app", "FixedDecimalPlaces") => I4(2),
        ("app", "MoveAfterReturnDirection") => I4(-4121),
        ("app", "Cursor" | "WindowState") => I4(-4143),
        ("app", "DefaultSaveFormat") => I4(51),
        ("app", "DisplayCommentIndicator") => I4(-1),
        ("app", "FormulaBarHeight" | "ReferenceStyle") => I4(1),
        ("app", "CalculationState") => I4(0),
        ("app", "UserName") => Bstr("zac".into()),
        ("app", "Name") => Bstr("Microsoft Excel".into()),
        ("app", "PathSeparator") => Bstr("\\".into()),
        ("app", "TemplatesPath") => {
            Bstr(r"C:\Users\zac\AppData\Roaming\Microsoft\Templates\".into())
        }
        ("wb", "InactiveListBorderVisible" | "KeepChangeHistory" | "EnableAutoRecover") => {
            Bool(true)
        }
        ("wb", "PersonalViewListSettings" | "PersonalViewPrintSettings") => Bool(true),
        ("wb", "AcceptLabelsInFormulas" | "Final") => Bool(false),
        ("wb", "ListChangesOnNewSheet" | "MultiUserEditing" | "ReadOnlyRecommended") => Bool(false),
        ("wb", "RemovePersonalInformation" | "ShowConflictHistory" | "TemplateRemoveExtData") => {
            Bool(false)
        }
        ("wb", "VBASigned" | "WriteReserved" | "EnvelopeVisible" | "Excel8CompatibilityMode") => {
            Bool(false)
        }
        ("wb", "HighlightChangesOnScreen" | "AutoSaveOn") => Bool(false),
        ("wb", "DisplayInkComments" | "ShowPivotTableFieldList" | "ChartDataPointTrack") => {
            Bool(true)
        }
        ("wb", "CheckCompatibility" | "ShowPivotChartActiveFields") => Bool(false),
        ("wb", "IsInplace" | "ConnectionsDisabled" | "CaseSensitive") => Bool(false),
        ("wb", "UseWholeCellCriteria" | "UseWildcards" | "PasswordEncryptionFileProperties") => {
            Bool(true)
        }
        ("wb", "PasswordEncryptionKeyLength") => I4(0),
        ("wb", "PasswordEncryptionAlgorithm") => Bstr(String::new()),
        ("app", "ChartDataPointTrack" | "EnableAnimations" | "CanPlaySounds") => Bool(true),
        ("app", "DisplayNoteIndicator" | "DisplayRecentFiles" | "CopyObjectsWithCells") => {
            Bool(true)
        }
        ("app", "MathCoprocessorAvailable" | "MouseAvailable" | "PivotTableSelection") => {
            Bool(true)
        }
        ("app", "ShowToolTips" | "AutoPercentEntry" | "Ready") => Bool(true),
        ("app", "CanRecordSounds" | "DisplayClipboardWindow" | "ConstrainNumeric") => Bool(false),
        ("app", "DisplayFullScreen" | "DisplayExcel4Menus" | "EnableSound") => Bool(false),
        ("app", "PromptForSummaryInfo" | "RecordRelative" | "TransitionNavigKeys") => Bool(false),
        ("app", "UserControl" | "WindowsForPens") => Bool(false),
        ("app", "DisplayFunctionToolTips" | "DisplayInsertOptions" | "GenerateGetPivotData") => {
            Bool(true)
        }
        (
            "app",
            "AutoFormatAsYouTypeReplaceHyperlinks"
            | "MapPaperSize"
            | "ArbitraryXMLSupportAvailable",
        ) => Bool(true),
        ("app", "ShowSelectionFloaties" | "ShowMenuFloaties" | "EnableLivePreview") => Bool(true),
        ("app", "WarnOnFunctionNameConflict" | "DisplayFormulaAutoComplete") => Bool(true),
        ("app", "EnableLargeOperationAlert" | "ShowQuickAnalysis" | "FlashFill") => Bool(true),
        ("app", "DisplayPasteOptions" | "ShowStartupDialog" | "ShowDevTools") => Bool(false),
        (
            "app",
            "DisplayDocumentActionTaskPane"
            | "DisplayDocumentInformationPanel"
            | "AlwaysUseClearType",
        ) => Bool(false),
        ("app", "DeferAsyncQueries" | "HighQualityModeForGraphics" | "UseClusterConnector") => {
            Bool(false)
        }
        ("app", "IsSandboxed" | "EnableMacroAnimations" | "FlashFillMode") => Bool(false),
        ("app", "AutomationSecurity" | "MeasurementUnit" | "GenerateTableRefs") => I4(1),
        ("app", "LargeOperationCellThousandCount") => I4(33554),
        ("app", "FileValidation" | "FileValidationPivot") => I4(0),
        ("app", "ClusterConnector") => Bstr(String::new()),
        ("app", "MergeInstances" | "EnableCheckFileExtensions" | "ShowConvertToDataType") => {
            Bool(true)
        }
        ("wb", "RevisionNumber") => I4(0),
        ("wb", "WriteReservedBy") => Bstr("zac".into()),
        ("app", "CommandUnderlines") => I4(-4105),
        ("app", "DataEntryMode") => I4(-4146),
        ("app", "EnableCancelKey") => I4(1),
        ("app", "ODBCTimeout") => I4(45),
        ("app", "DefaultSheetDirection") => I4(-5003),
        ("app", "FeatureInstall") => I4(0),
        ("app", "CustomListCount") => R8(8.0),
        ("app", "SheetsInNewWorkbook") => R8(2.0),
        ("app", "StandardFontSize") => R8(11.0),
        ("app", "TransitionMenuKeyAction" | "CursorMovement" | "MailSystem") => R8(1.0),
        ("app", "DDEAppReturnCode" | "ControlCharacters") => R8(0.0),
        ("app", "NetworkTemplatesPath" | "OrganizationName") => Bstr(String::new()),
        ("app", "StandardFont") => Bstr("Aptos Narrow".into()),
        ("app", "TransitionMenuKey") => Bstr("/".into()),
        ("app", "Path") => Bstr(r"C:\Program Files\Microsoft Office\Root\Office16".into()),
        ("app", "ProductCode") => Bstr("{90160000-000F-0000-1000-0000000FF1CE}".into()),
        ("app", "ActivePrinter") => Bstr("Microsoft Print to PDF on Ne01:".into()),
        ("app", "_Default" | "Value") => Bstr("Microsoft Excel".into()),
        ("wb", "AutoUpdateFrequency" | "ChangeHistoryDuration") => I4(0),
        ("wb", "ConflictResolution" | "UpdateLinks") => I4(1),
        ("wb", "DisplayDrawingObjects") => I4(-4104),
        ("wb", "CalculationVersion") => I4(191_029),
        (_, "Creator") => I4(1_480_803_660),
        _ => return None,
    })
}

impl Application<'_> {
    pub(crate) fn stored_option(
        &mut self,
        book: Option<BookId>,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        if let Some(r) = self.host_state_member(book, m, a) {
            return Some(r);
        }
        let scope = if book.is_some() { "wb" } else { "app" };
        let def = default_of(scope, m.name)?;
        let key = format!("{}:{}", book.map_or(0, |b| b.0 + 1), m.name);
        if m.kind == InvokeKind::Put {
            if book.is_some() {
                if let Err(e) = self.coerce_wb_put(m, a) {
                    return Some(Err(e));
                }
            }

            if m.name == "ReferenceStyle" {
                let ok = matches!(a.put, Some(Variant::I4(1 | -4150)));
                if !ok && self.put_i32(a).is_ok() || matches!(a.put, Some(Variant::Bstr(_))) {
                    return Some(Ok(Variant::Empty));
                }
            }

            if m.name == "SheetsInNewWorkbook" {
                let n = match a.put {
                    Some(Variant::Bool(true)) => Some(1.0),
                    Some(Variant::Bool(false) | Variant::Bstr(_)) | None => None,
                    _ => self.put_f64(a).ok().map(f64::trunc),
                };
                return Some(match n {
                    Some(n) if (1.0..=255.0).contains(&n) => {
                        self.opts.insert(key, Variant::R8(n));
                        Ok(Variant::Empty)
                    }
                    _ => Err(OmError::unable_to_set("SheetsInNewWorkbook", "Application")),
                });
            }
            let v = match &def {
                Variant::Bool(_) => self.put_bool(a).map(Variant::Bool),
                Variant::I4(_) => self.put_i32(a).map(Variant::I4),
                Variant::R8(_) => self.put_f64(a).map(Variant::R8),
                _ => self.put_string(a).map(Variant::Bstr),
            };
            return Some(v.map(|v| {
                self.opts.insert(key, v);
                Variant::Empty
            }));
        }
        Some(Ok(self.opts.get(&key).cloned().unwrap_or(def)))
    }

    fn host_state_member(
        &mut self,
        book: Option<BookId>,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        use Variant::{Bool, Bstr, Null, R8};
        if m.kind != InvokeKind::Get {
            return None;
        }
        let list = |v: Vec<Variant>| {
            Variant::Array(Box::new(xlcalc_variant::SafeArray {
                elem: xlcalc_variant::VarType::Variant,
                bounds: vec![(1, v.len() as u32)],
                data: v,
            }))
        };
        Some(match (book.is_some(), m.name) {
            (false, "FileConverters" | "MailSession" | "RegisteredFunctions") => Ok(Null),
            (false, "OnWindow") => Ok(Variant::Empty),
            (false, "PreviousSelections") => Ok(Variant::Dispatch(None)),
            (false, "Hinstance") => Err(OmError::exception(0x8000_FFFF_u32 as i32, None)),
            (false, "ActiveEncryptionSession") => {
                Err(OmError::exception(0x8007_0057_u32 as i32, None))
            }
            (false, "DecimalSeparator" | "ThousandsSeparator") => Ok(Bstr(",".into())),

            (false, "FileDialog") => a
                .i32(0)
                .map_err(|_| OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))
                .and_then(|n| match n {
                    Some(1..=4) => Ok(self.alloc(Object::Stub("FileDialog"))),
                    _ => Err(OmError::exception(0x8000_4005_u32 as i32, None)),
                }),
            (false, "ActiveProtectedViewWindow") | (true, "CommandBars") => {
                Ok(Variant::Dispatch(None))
            }
            (false, "SensitivityLabelPolicy") => {
                Err(OmError::exception(0x8004_030A_u32 as i32, None))
            }
            (true, "Mailer") => Err(OmError::app_defined()),
            (false, "International") => {
                let all = international();
                a.i32(0).and_then(|i| match i {
                    None => Ok(list(all)),
                    Some(i) => usize::try_from(i - 1)
                        .ok()
                        .and_then(|i| all.get(i).cloned())
                        .ok_or_else(OmError::app_defined),
                })
            }
            (true, "Container") => Err(OmError::app_defined()),
            (true, "CreateBackup") => Ok(Bool(false)),
            (true, "PivotTables") => Ok(self.alloc(Object::Stub("PivotTables"))),
            (true, "Colors") => {
                let colors: Vec<Variant> =
                    (1..=56).map(|i| R8(f64::from(palette_bgr(i)))).collect();
                a.i32(0).and_then(|i| match i {
                    None => Ok(list(colors)),
                    Some(i) => usize::try_from(i - 1)
                        .ok()
                        .and_then(|i| colors.get(i).cloned())
                        .ok_or_else(OmError::app_defined),
                })
            }
            _ => return None,
        })
    }

    pub(crate) fn sheet_stored_option(
        &mut self,
        book: BookId,
        sheet: xlcalc::SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        use Variant::{Bool, Bstr, I4};
        let (def, text_is_1004) = match m.name {
            "EnableOutlining"
            | "EnablePivotTable"
            | "EnableAutoFilter"
            | "TransitionFormEntry"
            | "TransitionExpEval"
            | "DisplayRightToLeft" => (Bool(false), true),
            "DisplayPageBreaks" => (Bool(false), false),
            "EnableFormatConditionsCalculation" | "EnableCalculation" => (Bool(true), false),
            "ScrollArea" => (Bstr(String::new()), true),
            "PrintedCommentPages" => (I4(0), false),
            "ConsolidationFunction" => (I4(-4157), false),
            "ConsolidationOptions" => {
                return Some(match m.kind {
                    InvokeKind::Get => Ok(Variant::Array(Box::new(xlcalc_variant::SafeArray {
                        elem: xlcalc_variant::VarType::Variant,
                        bounds: vec![(1, 3)],
                        data: vec![Bool(false), Bool(false), Bool(false)],
                    }))),
                    _ => Err(OmError::app_defined()),
                });
            }
            "ConsolidationSources" => {
                return Some(match m.kind {
                    InvokeKind::Get => Ok(Variant::Empty),
                    _ => Err(OmError::app_defined()),
                });
            }
            _ => return None,
        };
        let key = format!("ws{}:{}:{}", book.0, sheet.0, m.name);
        if m.kind != InvokeKind::Put {
            return Some(Ok(self.opts.get(&key).cloned().unwrap_or(def)));
        }
        if text_is_1004 && matches!(a.put, Some(Variant::Bstr(_))) && !matches!(def, Bstr(_)) {
            return Some(Err(OmError::app_defined()));
        }
        if let Err(e) = self.coerce_wb_put(m, a) {
            return Some(Err(e));
        }
        let fail = |app: &Self| {
            if text_is_1004 || matches!(a.put, Some(Variant::Bstr(_))) && app.put_bool(a).is_err() {
                OmError::app_defined()
            } else {
                OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0)
            }
        };
        let v = match &def {
            Bool(_) => match self.put_bool(a) {
                Ok(b) => Bool(b),
                Err(_) => return Some(Err(fail(self))),
            },
            Bstr(_) => {
                let text = match a.put {
                    Some(Variant::Bstr(ref s)) => s.clone(),
                    _ => return Some(Err(OmError::app_defined())),
                };
                if text.is_empty() {
                    Bstr(text)
                } else {
                    let wb = &self.book(book).ok()?.wb;
                    match crate::addr::parse_range_text(wb, sheet, &text) {
                        Some((s, rect)) if s == sheet => Bstr(crate::addr::range_text(
                            rect,
                            &crate::addr::AddressStyle {
                                row_abs: true,
                                col_abs: true,
                                r1c1: false,
                                relative_to: (0, 0),
                            },
                        )),
                        _ => return Some(Err(OmError::app_defined())),
                    }
                }
            }
            _ => return Some(Err(OmError::app_defined())),
        };
        self.opts.insert(key, v);
        Some(Ok(Variant::Empty))
    }
}
