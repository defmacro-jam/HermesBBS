#!/usr/bin/env python3
"""Compare Pascal and Lisp record definitions for Hermes BBS."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Sequence, Tuple, Union

RECORD_MAP = {
    "user": ("UserRec", "user-record"),
    "message": ("MesgRec", "message-record"),
    "system": ("SystRec", "system-record"),
}

PASCAL_CONSTANTS = {
    "MAX_NODES": 10,
    "MAX_NODES_M_1": 9,
    "MAX_RESTRICTIONS": 26,
}

LISP_CONSTANTS = {
    "+max-nodes+": 10,
    "+max-nodes-minus-one+": 9,
    "+max-restrictions+": 26,
    "+max-message-forums+": 20,
    "+max-conferences-per-forum+": 50,
}

Scalar = Union[str, Tuple[Any, ...]]


@dataclass
class FieldDefinition:
    name: str
    canonical_type: Scalar
    raw_type: str


def strip_pascal_comments(text: str) -> str:
    result: List[str] = []
    depth = 0
    i = 0
    while i < len(text):
        ch = text[i]
        if depth > 0:
            if ch == '}':
                depth -= 1
            elif ch == '{':
                depth += 1
            if ch == '\n':
                result.append('\n')
            i += 1
            continue
        if ch == '{':
            depth += 1
            result.append(' ')
            i += 1
        else:
            result.append(ch)
            i += 1
    cleaned = ''.join(result)
    processed_lines: List[str] = []
    for line in cleaned.splitlines():
        in_string = False
        j = 0
        while j < len(line):
            char = line[j]
            if char == "'":
                in_string = not in_string
                j += 1
                continue
            if not in_string and char == '/' and j + 1 < len(line) and line[j + 1] == '/':
                line = line[:j]
                break
            j += 1
        processed_lines.append(line)
    return '\n'.join(processed_lines)


def find_pascal_record(text: str, name: str) -> str:
    lower_text = text.lower()
    target = f"{name.lower()} = record"
    pos = lower_text.find(target)
    while pos != -1 and pos > 0 and lower_text[pos - 1].isalnum():
        pos = lower_text.find(target, pos + 1)
    if pos == -1:
        raise ValueError(f"Unable to locate Pascal record {name}.")
    record_index = lower_text.find('record', pos)
    start = record_index + len('record')
    depth = 1
    i = start
    while i < len(text) and depth > 0:
        if re.match(r"\\brecord\\b", text[i:], re.IGNORECASE):
            depth += 1
            i += len("record")
            continue
        if text[i:i + 4].lower() == "end;":
            depth -= 1
            i += 4
            continue
        i += 1
    if depth != 0:
        raise ValueError(f"Unbalanced record definition for {name}.")
    body = text[start:i - 4]
    return body.strip()


def parse_range_length(range_expr: str) -> int:
    low_str, high_str = [part.strip() for part in range_expr.split('..')]
    low = resolve_pascal_constant(low_str)
    high = resolve_pascal_constant(high_str)
    return high - low + 1


def resolve_pascal_constant(token: str) -> int:
    token_upper = token.upper()
    if token_upper in PASCAL_CONSTANTS:
        return PASCAL_CONSTANTS[token_upper]
    try:
        return int(token)
    except ValueError as exc:  # pragma: no cover - defensive
        raise ValueError(f"Unknown Pascal constant {token}.") from exc


def canonical_struct_name(name: str) -> str:
    base = name.strip().replace('_', '-')
    base = re.sub(r'([a-z0-9])([A-Z])', r'\1-\2', base)
    base = re.sub(r'([A-Z]+)([A-Z][a-z])', r'\1-\2', base)
    return base.lower()


def canonical_pascal_type(type_expr: str) -> Scalar:
    expr = type_expr.strip()
    lower = expr.lower()
    string_match = re.match(r"string\[(\d+)\]", lower)
    if string_match:
        return ("pascal-string", int(string_match.group(1)))
    if lower.startswith("str") and lower[3:].isdigit():
        return ("pascal-string", int(lower[3:]))
    if lower in {"boolean", "byte", "char", "integer", "longint", "real"}:
        mapping = {
            "boolean": "boolean",
            "byte": "uint8",
            "char": "char",
            "integer": "int16",
            "longint": "int32",
            "real": "real",
        }
        return mapping[lower]
    if lower == "signedbyte":
        return "int8"
    if lower.startswith("packed array") or lower.startswith("array"):
        packed = lower.startswith("packed array")
        start = len("packed array") if packed else len("array")
        remainder = expr[start:].strip()
        dim_part, of_part = remainder.split(']', 1)
        dim_tokens = [token.strip() for token in dim_part[1:].split(',')]
        subtype_expr = of_part.split('of', 1)[1].strip()
        lengths = [parse_range_length(token) for token in dim_tokens]
        subtype = canonical_pascal_type(subtype_expr)
        if len(lengths) == 1:
            length = lengths[0]
            if subtype == "boolean":
                return ("boolean-array", length)
            if subtype in {"uint8", "char"}:
                return ("byte-array", length)
            return ("array", length, subtype)
        if len(lengths) == 2:
            rows, cols = lengths
            if subtype == "boolean":
                base = "boolean"
            elif subtype in {"uint8", "char"}:
                base = "uint8"
            else:
                base = subtype
            return ("matrix", rows, cols, base)
        raise ValueError(f"Unsupported Pascal array dimension {dim_tokens}.")
    if lower.startswith('(') and lower.endswith(')'):
        return "int16"
    if lower == "rect":
        return ("struct", "rect")
    if lower.endswith("rec"):
        return ("struct", canonical_struct_name(expr))
    raise ValueError(f"Unhandled Pascal type expression: {type_expr}")


def parse_pascal_fields(body: str) -> List[FieldDefinition]:
    fields: List[FieldDefinition] = []
    for chunk in body.split(';'):
        entry = chunk.strip()
        if not entry:
            continue
        if ':' not in entry:
            continue
        names_part, type_part = entry.split(':', 1)
        raw_type = type_part.strip()
        for name in names_part.split(','):
            cleaned = name.strip()
            if cleaned:
                fields.append(FieldDefinition(cleaned, canonical_pascal_type(raw_type), raw_type))
    return fields


def remove_lisp_comments(text: str) -> str:
    cleaned = []
    for line in text.splitlines():
        stripped = line.split(';', 1)[0]
        cleaned.append(stripped)
    return '\n'.join(cleaned)


def find_lisp_record(text: str, name: str) -> str:
    pattern = f"(define-pascal-record {name}"
    start = text.find(pattern)
    if start == -1:
        raise ValueError(f"Unable to locate Lisp record {name}.")
    depth = 0
    i = start
    while i < len(text):
        ch = text[i]
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
            if depth == 0:
                return text[start:i + 1]
        i += 1
    raise ValueError(f"Unbalanced Lisp form for {name}.")


def tokenize_lisp(source: str) -> List[Union[str, int]]:
    tokens: List[Union[str, int]] = []
    i = 0
    while i < len(source):
        ch = source[i]
        if ch.isspace():
            i += 1
            continue
        if ch in '()':
            tokens.append(ch)
            i += 1
            continue
        if ch == '"':
            i += 1
            start = i
            while i < len(source) and source[i] != '"':
                if source[i] == '\\':
                    i += 2
                else:
                    i += 1
            tokens.append(source[start:i])
            i += 1
            continue
        start = i
        while i < len(source) and not source[i].isspace() and source[i] not in '()':
            i += 1
        token = source[start:i]
        if re.fullmatch(r"[-+]?\d+", token):
            tokens.append(int(token))
        else:
            tokens.append(token)
    return tokens


def parse_tokens(tokens: List[Union[str, int]]) -> Any:
    if not tokens:
        raise ValueError("Unexpected end of tokens.")
    token = tokens.pop(0)
    if token == '(':
        result = []
        while tokens and tokens[0] != ')':
            result.append(parse_tokens(tokens))
        if not tokens:
            raise ValueError("Missing closing parenthesis.")
        tokens.pop(0)
        return result
    if token == ')':  # pragma: no cover - defensive
        raise ValueError("Unexpected closing parenthesis.")
    return token


def parse_lisp_form(form: Any) -> List[FieldDefinition]:
    if not isinstance(form, list) or len(form) < 4:
        raise ValueError("Malformed define-pascal-record form.")
    field_forms = form[3:]
    fields: List[FieldDefinition] = []
    for entry in field_forms:
        if not isinstance(entry, list) or len(entry) < 2:
            continue
        name = str(entry[0])
        type_spec = entry[1]
        fields.append(FieldDefinition(name, canonical_lisp_type(type_spec), repr(type_spec)))
    return fields


def eval_lisp_expr(expr: Any) -> int:
    if isinstance(expr, int):
        return expr
    if isinstance(expr, str):
        if expr in LISP_CONSTANTS:
            return LISP_CONSTANTS[expr]
        if re.fullmatch(r"[-+]?\d+", expr):
            return int(expr)
        raise ValueError(f"Unknown Lisp constant {expr}.")
    if isinstance(expr, list):
        if not expr:
            raise ValueError("Empty expression.")
        op = expr[0]
        args = [eval_lisp_expr(arg) for arg in expr[1:]]
        if op == '+':
            return sum(args)
        if op == '-':
            if not args:
                raise ValueError("Unary subtraction requires an argument.")
            value = args[0]
            for item in args[1:]:
                value -= item
            return value
        raise ValueError(f"Unsupported Lisp operator {op}.")
    raise ValueError(f"Unsupported Lisp expression {expr}.")


def canonical_lisp_type(spec: Any) -> Scalar:
    if isinstance(spec, str):
        mapping = {
            ':int16': 'int16',
            ':uint16': 'uint16',
            ':int32': 'int32',
            ':uint32': 'uint32',
            ':int8': 'int8',
            ':uint8': 'uint8',
            ':boolean': 'boolean',
            ':byte': 'uint8',
            ':char': 'char',
            ':real': 'real',
        }
        if spec in mapping:
            return mapping[spec]
        raise ValueError(f"Unknown Lisp scalar type {spec}.")
    if isinstance(spec, list) and spec:
        head = spec[0]
        if head == ':pascal-string':
            return ('pascal-string', eval_lisp_expr(spec[1]))
        if head == ':boolean-array':
            return ('boolean-array', eval_lisp_expr(spec[1]))
        if head == ':byte-array':
            return ('byte-array', eval_lisp_expr(spec[1]))
        if head == ':int16-array':
            return ('array', eval_lisp_expr(spec[1]), 'int16')
        if head == ':int32-array':
            return ('array', eval_lisp_expr(spec[1]), 'int32')
        if head == ':array':
            length = eval_lisp_expr(spec[1])
            subtype = canonical_lisp_type(spec[2])
            return ('array', length, subtype)
        if head == ':matrix':
            rows = eval_lisp_expr(spec[1])
            cols = eval_lisp_expr(spec[2])
            subtype = canonical_lisp_type(spec[3])
            return ('matrix', rows, cols, subtype)
        if head == ':struct':
            return ('struct', canonical_struct_name(str(spec[1])))
        if head == ':reserved':
            return ('byte-array', eval_lisp_expr(spec[1]))
    raise ValueError(f"Unsupported Lisp type specification {spec}.")


def normalize_name_for_display(name: str) -> str:
    return name.strip()


def normalized_for_comparison(name: str) -> str:
    return re.sub(r"[^a-z0-9]", '', name.lower())


def format_type(canonical: Scalar) -> str:
    if isinstance(canonical, str):
        return canonical
    head = canonical[0]
    if head == 'pascal-string':
        return f"pascal-string({canonical[1]})"
    if head in {'boolean-array', 'byte-array'}:
        return f"{head}[{canonical[1]}]"
    if head == 'array':
        return f"array[{canonical[1]}] of {format_type(canonical[2])}"
    if head == 'matrix':
        return f"matrix[{canonical[1]}x{canonical[2]}] of {format_type(canonical[3])}"
    if head == 'struct':
        return f"struct {canonical[1]}"
    return str(canonical)


def compare_records(pascal_fields: List[FieldDefinition],
                    lisp_fields: List[FieldDefinition]) -> Tuple[List[str], List[Tuple[str, str, str]]]:
    errors: List[str] = []
    name_diffs: List[Tuple[str, str, str]] = []
    if len(pascal_fields) != len(lisp_fields):
        errors.append(f"Field count mismatch (Pascal {len(pascal_fields)} vs Lisp {len(lisp_fields)}).")
    for index, (p_field, l_field) in enumerate(zip(pascal_fields, lisp_fields)):
        if p_field.canonical_type != l_field.canonical_type:
            errors.append(
                f"Field {index + 1} type mismatch: Pascal {p_field.name} -> {format_type(p_field.canonical_type)}"
                f", Lisp {l_field.name} -> {format_type(l_field.canonical_type)}"
            )
        if normalized_for_comparison(p_field.name) != normalized_for_comparison(l_field.name):
            name_diffs.append((str(index + 1), p_field.name, l_field.name))
    return errors, name_diffs


def load_pascal_fields(path: Path, name: str) -> List[FieldDefinition]:
    text = strip_pascal_comments(path.read_text(encoding='utf-8'))
    body = find_pascal_record(text, name)
    return parse_pascal_fields(body)


def load_lisp_fields(path: Path, name: str) -> List[FieldDefinition]:
    raw = remove_lisp_comments(path.read_text(encoding='utf-8'))
    form_text = find_lisp_record(raw, name)
    tokens = tokenize_lisp(form_text)
    parsed = parse_tokens(tokens)
    return parse_lisp_form(parsed)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Compare Pascal and Lisp record layouts.")
    parser.add_argument('--records', default='user,message,system',
                        help='Comma separated list of records to check (user,message,system).')
    parser.add_argument('--pascal', default='Source/Initial.p',
                        help='Path to the Pascal source containing record definitions.')
    parser.add_argument('--lisp', default='lisp/storage.lisp',
                        help='Path to the Lisp storage definitions.')
    parser.add_argument('--verbose', action='store_true', help='Show detailed field mappings.')
    args = parser.parse_args(argv)

    requested = [item.strip().lower() for item in args.records.split(',') if item.strip()]
    pascal_path = Path(args.pascal)
    lisp_path = Path(args.lisp)

    exit_code = 0
    for key in requested:
        if key not in RECORD_MAP:
            print(f"Unknown record '{key}'. Valid options: {', '.join(sorted(RECORD_MAP))}.", file=sys.stderr)
            return 1
        pascal_name, lisp_name = RECORD_MAP[key]
        pascal_fields = load_pascal_fields(pascal_path, pascal_name)
        lisp_fields = load_lisp_fields(lisp_path, lisp_name)
        errors, name_diffs = compare_records(pascal_fields, lisp_fields)
        print(f"Record '{key}': compared {min(len(pascal_fields), len(lisp_fields))} fields.")
        if errors:
            exit_code = 1
            print("  Type differences detected:")
            for error in errors:
                print(f"    - {error}")
        else:
            print("  Type parity verified.")
        if name_diffs:
            print("  Name variations:")
            for index, pascal, lisp in name_diffs:
                print(f"    #{index}: Pascal '{pascal}' vs Lisp '{lisp}'")
        elif args.verbose:
            print("  Field names align.")
        if args.verbose and not name_diffs:
            for idx, (p_field, l_field) in enumerate(zip(pascal_fields, lisp_fields), start=1):
                print(f"    {idx:3d}: {p_field.name} -> {l_field.name} ({format_type(p_field.canonical_type)})")
    return exit_code


if __name__ == '__main__':
    sys.exit(main())
