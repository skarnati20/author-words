#!/usr/bin/env python3
import sys
import os
import re
import html
import struct
import zipfile
import subprocess
import xml.etree.ElementTree as ET

def strip_html_tags(html_str):
    if not html_str:
        return ""
    # Replace block level tags with newlines
    s = re.sub(r"(?i)<(p|br|div|h[1-6]|li|tr|blockquote)[^>]*>", "\n", html_str)
    # Strip script and style blocks
    s = re.sub(r"(?is)<(script|style)[^>]*>.*?</\1>", "", s)
    # Strip all remaining tags
    s = re.sub(r"<[^>]+>", "", s)
    # Unescape HTML entities
    s = html.unescape(s)
    # Normalize spaces
    s = re.sub(r"[ \t]+", " ", s)
    s = re.sub(r"\n\s*\n+", "\n\n", s)
    return s.strip()

def extract_epub(path):
    with zipfile.ZipFile(path, "r") as z:
        names = z.namelist()
        
        # 1. Try to find the OPF file to read chapters in spine order
        opf_path = None
        if "META-INF/container.xml" in names:
            try:
                container_data = z.read("META-INF/container.xml")
                root = ET.fromstring(container_data)
                for elem in root.iter():
                    if elem.tag.endswith("rootfile"):
                        opf_path = elem.attrib.get("full-path")
                        break
            except Exception:
                pass
        
        ordered_files = []
        if opf_path and opf_path in names:
            try:
                opf_dir = os.path.dirname(opf_path)
                opf_data = z.read(opf_path)
                root = ET.fromstring(opf_data)
                
                manifest = {}
                for item in root.iter():
                    if item.tag.endswith("item"):
                        item_id = item.attrib.get("id")
                        href = item.attrib.get("href")
                        if item_id and href:
                            full_href = os.path.normpath(os.path.join(opf_dir, href)).replace("\\", "/")
                            manifest[item_id] = full_href
                
                for itemref in root.iter():
                    if itemref.tag.endswith("itemref"):
                        idref = itemref.attrib.get("idref")
                        if idref in manifest:
                            ordered_files.append(manifest[idref])
            except Exception:
                pass
        
        # If spine ordering failed, fallback to all html/xhtml files
        if not ordered_files:
            ordered_files = [n for n in names if n.lower().endswith(('.xhtml', '.html', '.htm'))]
        
        chapter_texts = []
        for name in ordered_files:
            if name in names:
                try:
                    data = z.read(name).decode("utf-8", errors="replace")
                    clean = strip_html_tags(data)
                    if clean:
                        chapter_texts.append(clean)
                except Exception:
                    continue
        
        full_text = "\n\n".join(chapter_texts)
        if full_text.strip():
            return full_text
        
        # If no text was found, check if it is an image-based/scanned EPUB
        image_extensions = ('.jpg', '.jpeg', '.png', '.webp', '.tiff', '.bmp')
        image_names = [n for n in names if n.lower().endswith(image_extensions)]
        if image_names:
            # Sort images and run Vision OCR on each image if available
            bin_dir = os.path.dirname(os.path.abspath(__file__))
            ocr_tool = os.path.join(bin_dir, "ocr-pdf")
            ocr_results = []
            import tempfile
            with tempfile.TemporaryDirectory() as tmpdir:
                for img_name in sorted(image_names):
                    tmp_img = os.path.join(tmpdir, os.path.basename(img_name))
                    with open(tmp_img, "wb") as f:
                        f.write(z.read(img_name))
                    if os.path.exists(ocr_tool):
                        res = subprocess.run([ocr_tool, tmp_img], capture_output=True, text=True)
                        if res.stdout.strip():
                            ocr_results.append(res.stdout.strip())
            if ocr_results:
                return "\n\n".join(ocr_results)
                
        return full_text

def uncompress_palmdoc(data):
    length = len(data)
    offset = 0
    out = bytearray()
    while offset < length:
        b = data[offset]
        offset += 1
        if b == 0:
            out.append(0)
        elif b <= 8:
            out.extend(data[offset:offset+b])
            offset += b
        elif b <= 0x7f:
            out.append(b)
        elif b <= 0xbf:
            if offset >= length:
                break
            pair = struct.unpack(">H", data[offset-1:offset+1])[0]
            offset += 1
            pair &= 0x3fff
            match_len = (pair & 0x07) + 3
            match_dist = pair >> 3
            if match_dist < 1 or match_dist > len(out):
                continue
            for _ in range(match_len):
                out.append(out[-match_dist])
        else:
            out.append(0x20) # space
            out.append(b ^ 0x80)
    return bytes(out)

def extract_azw3_mobi(path):
    with open(path, "rb") as f:
        data = f.read()
    if len(data) < 78:
        raise ValueError("Invalid file: too small to be a Palm Database")
    
    num_records, = struct.unpack(">H", data[76:78])
    record_offsets = []
    for i in range(num_records):
        off, = struct.unpack(">I", data[78 + i*8 : 82 + i*8])
        record_offsets.append(off)
    record_offsets.append(len(data))
    
    if num_records < 1:
        return ""
    
    # Record 0 has PalmDOC / MOBI header
    r0 = data[record_offsets[0]:record_offsets[1]]
    compression, = struct.unpack(">H", r0[0:2])
    text_record_count, = struct.unpack(">H", r0[8:10])
    
    # Check for DRM
    encryption_type, = struct.unpack(">H", r0[12:14])
    if encryption_type != 0:
        raise PermissionError("The Kindle file is protected by Amazon DRM encryption and cannot be unpacked.")
    
    text_bytes = bytearray()
    for i in range(1, min(text_record_count + 1, num_records)):
        rec_data = data[record_offsets[i]:record_offsets[i+1]]
        if compression == 1:
            text_bytes.extend(rec_data)
        elif compression == 2:
            text_bytes.extend(uncompress_palmdoc(rec_data))
        else:
            text_bytes.extend(rec_data)
    
    raw_html = text_bytes.decode("utf-8", errors="replace")
    return strip_html_tags(raw_html)

def extract_kfx(path):
    # Try ebook-convert if installed
    try:
        import tempfile
        with tempfile.NamedTemporaryFile(suffix=".txt") as tmp:
            res = subprocess.run(["ebook-convert", path, tmp.name], capture_output=True, text=True)
            if res.returncode == 0 and os.path.exists(tmp.name):
                with open(tmp.name, "r", encoding="utf-8", errors="replace") as f:
                    return f.read()
    except Exception:
        pass
    
    # Check if file has DRM or inspect container
    with open(path, "rb") as f:
        header = f.read(4096)
    if b"DRM" in header or b"$418" in header:
        raise PermissionError("The KFX file is protected by Amazon DRM encryption.")
    
    # For unencrypted KFX, extract text from Ion strings
    # Extract strings of printable length >= 3
    strings = []
    with open(path, "rb") as f:
        data = f.read()
    # Find UTF-8 printable runs
    for m in re.finditer(b'[\x20-\x7E\xC2-\xF4][\x20-\x7E\x80-\xBF]{3,}', data):
        try:
            s = m.group(0).decode("utf-8")
            # Filter out format tags
            if not s.startswith(("$", "com.amazon", "kfx")):
                strings.append(s)
        except Exception:
            continue
    return "\n\n".join(strings)

def main():
    if len(sys.argv) < 2:
        print("Usage: extract-ebook.py <file-path>", file=sys.stderr)
        sys.exit(1)
    
    path = sys.argv[1]
    ext = os.path.splitext(path)[1].lower().lstrip(".")
    
    try:
        if ext == "epub":
            print(extract_epub(path))
        elif ext in ("azw3", "azw", "mobi"):
            print(extract_azw3_mobi(path))
        elif ext == "kfx":
            print(extract_kfx(path))
        else:
            with open(path, "r", encoding="utf-8", errors="replace") as f:
                print(f.read())
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
