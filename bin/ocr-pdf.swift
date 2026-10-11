import Foundation
import PDFKit
import Vision
import AppKit

guard CommandLine.arguments.count > 1 else {
    fputs("Usage: ocr-pdf <file-path>\n", stderr)
    exit(1)
}

let filePath = CommandLine.arguments[1]
let url = URL(fileURLWithPath: filePath)

// 1. Try opening as PDF
if let doc = PDFDocument(url: url) {
    var fullText = ""
    var hasDigitalText = false

    for i in 0..<doc.pageCount {
        if let page = doc.page(at: i), let pageText = page.string, !pageText.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            fullText += pageText + "\n\n"
            hasDigitalText = true
        }
    }

    // If digital text was found and is substantial, output it
    if hasDigitalText && fullText.count > doc.pageCount * 20 {
        print(fullText)
        exit(0)
    }

    // Otherwise run Apple Vision OCR on rendered page images
    var ocrText = ""
    for i in 0..<doc.pageCount {
        guard let page = doc.page(at: i) else { continue }
        let pageRect = page.bounds(for: .mediaBox)
        let targetSize = CGSize(width: max(pageRect.width * 2, 1400), height: max(pageRect.height * 2, 1800))
        let img = page.thumbnail(of: targetSize, for: .mediaBox)
        guard let cgImg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) else { continue }
        
        var pageLines: [String] = []
        let pageRequest = VNRecognizeTextRequest { req, _ in
            guard let observations = req.results as? [VNRecognizedTextObservation] else { return }
            pageLines = observations.compactMap { $0.topCandidates(1).first?.string }
        }
        pageRequest.recognitionLevel = .accurate
        pageRequest.usesLanguageCorrection = true
        
        let handler = VNImageRequestHandler(cgImage: cgImg, options: [:])
        try? handler.perform([pageRequest])
        
        ocrText += pageLines.joined(separator: "\n") + "\n\n"
    }

    print(ocrText)
    exit(0)
}

// 2. If not a PDF, try opening as a standalone image file (PNG, JPG, TIFF, etc.)
if let img = NSImage(contentsOf: url),
   let cgImg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) {
    var imageLines: [String] = []
    let imgRequest = VNRecognizeTextRequest { req, _ in
        guard let observations = req.results as? [VNRecognizedTextObservation] else { return }
        imageLines = observations.compactMap { $0.topCandidates(1).first?.string }
    }
    imgRequest.recognitionLevel = .accurate
    imgRequest.usesLanguageCorrection = true
    
    let handler = VNImageRequestHandler(cgImage: cgImg, options: [:])
    try? handler.perform([imgRequest])
    
    print(imageLines.joined(separator: "\n"))
    exit(0)
}

fputs("Error: Could not open file as PDF or image: \(filePath)\n", stderr)
exit(1)
