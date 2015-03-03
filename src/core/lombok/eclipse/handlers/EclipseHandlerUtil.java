/*
 * Copyright (C) 2009-2015 The Project Lombok Authors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package lombok.eclipse.handlers;

import static lombok.core.handlers.HandlerUtil.*;
import static lombok.eclipse.Eclipse.*;
import static lombok.eclipse.EclipseAugments.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.AccessLevel;
import lombok.ConfigurationKeys;
import lombok.Data;
import lombok.Getter;
import lombok.Lombok;
import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.core.AnnotationValues.AnnotationValue;
import lombok.core.TypeResolver;
import lombok.core.configuration.NullCheckExceptionType;
import lombok.core.debug.FileLog;
import lombok.core.debug.ProblemReporter;
import lombok.core.handlers.HandlerUtil;
import lombok.eclipse.Eclipse;
import lombok.eclipse.EclipseAST;
import lombok.eclipse.EclipseNode;
import lombok.experimental.Accessors;
import lombok.experimental.Tolerate;

import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.AbstractVariableDeclaration;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Block;
import org.eclipse.jdt.internal.compiler.ast.CastExpression;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration;
import org.eclipse.jdt.internal.compiler.ast.EqualExpression;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.FieldReference;
import org.eclipse.jdt.internal.compiler.ast.IfStatement;
import org.eclipse.jdt.internal.compiler.ast.IntLiteral;
import org.eclipse.jdt.internal.compiler.ast.MarkerAnnotation;
import org.eclipse.jdt.internal.compiler.ast.MemberValuePair;
import org.eclipse.jdt.internal.compiler.ast.MessageSend;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.NameReference;
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation;
import org.eclipse.jdt.internal.compiler.ast.NullLiteral;
import org.eclipse.jdt.internal.compiler.ast.OperatorIds;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedNameReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleMemberAnnotation;
import org.eclipse.jdt.internal.compiler.ast.SingleNameReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Statement;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.ThisReference;
import org.eclipse.jdt.internal.compiler.ast.ThrowStatement;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeParameter;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.CaptureBinding;
import org.eclipse.jdt.internal.compiler.lookup.ParameterizedTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.lookup.WildcardBinding;

/**
 * Container for static utility methods useful to handlers written for eclipse.
 */
public class EclipseHandlerUtil {
	private EclipseHandlerUtil() {
		// Prevent instantiation
	}
	
	/**
	 * Generates an error in the Eclipse error log. Note that most people never
	 * look at it!
	 * 
	 * @param cud
	 *            The {@code CompilationUnitDeclaration} where the error
	 *            occurred. An error will be generated on line 0 linking to the
	 *            error log entry. Can be {@code null}.
	 * @param message
	 *            Human readable description of the problem.
	 * @param ex
	 *            The associated exception. Can be {@code null}.
	 */
	public static void error(final CompilationUnitDeclaration cud, final String message, final Throwable ex) {
		ProblemReporter.error(message, ex);
		if (cud != null) EclipseAST.addProblemToCompilationResult(cud.getFileName(), cud.compilationResult, false, message + " - See error log.", 0, 0);
	}
	
	/**
	 * Generates a warning in the Eclipse error log. Note that most people never
	 * look at it!
	 * 
	 * @param message
	 *            Human readable description of the problem.
	 * @param ex
	 *            The associated exception. Can be {@code null}.
	 */
	public static void warning(final String message, final Throwable ex) {
		ProblemReporter.warning(message, ex);
	}
	
	public static ASTNode getGeneratedBy(final ASTNode node) {
		return ASTNode_generatedBy.get(node);
	}
	
	public static boolean isGenerated(final ASTNode node) {
		return getGeneratedBy(node) != null;
	}
	
	public static <T extends ASTNode> T setGeneratedBy(final T node, final ASTNode source) {
		ASTNode_generatedBy.set(node, source);
		return node;
	}
	
	public static MarkerAnnotation generateDeprecatedAnnotation(final ASTNode source) {
		final QualifiedTypeReference qtr = new QualifiedTypeReference(new char[][] { {'j', 'a', 'v', 'a'}, {'l', 'a', 'n', 'g'}, {'D', 'e', 'p', 'r', 'e', 'c', 'a', 't', 'e', 'd'}}, poss(source, 3));
		setGeneratedBy(qtr, source);
		final MarkerAnnotation ma = new MarkerAnnotation(qtr, source.sourceStart);
		// No matter what value you input for sourceEnd, the AST->DOM converter
		// of eclipse will reparse to find the end, and will fail as
		// it can't find code that isn't really there. This results in the end
		// position being set to 2 or 0 or some weird magic value, and thus,
		// length, as calculated by end-start, is all screwed up, resulting in
		// IllegalArgumentException during a setSourceRange call MUCH later in
		// the process.
		// We solve it by going with a voodoo magic source start value such that
		// the calculated length so happens to exactly be 0. 0 lengths are
		// accepted
		// by eclipse. For some reason.
		// TL;DR: Don't change 1. 1 is sacred. Trust the 1.
		// issue: #408.
		ma.sourceStart = 1;
		setGeneratedBy(ma, source);
		return ma;
	}
	
	public static boolean isFieldDeprecated(final EclipseNode fieldNode) {
		final FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		if ((field.modifiers & ClassFileConstants.AccDeprecated) != 0) {
			return true;
		}
		if (field.annotations == null) return false;
		for (final Annotation annotation : field.annotations) {
			if (typeMatches(Deprecated.class, fieldNode, annotation.type)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Checks if the given TypeReference node is likely to be a reference to the
	 * provided class.
	 * 
	 * @param type
	 *            An actual type. This method checks if {@code typeNode} is
	 *            likely to be a reference to this type.
	 * @param node
	 *            A Lombok AST node. Any node in the appropriate compilation
	 *            unit will do (used to get access to import statements).
	 * @param typeRef
	 *            A type reference to check.
	 */
	public static boolean typeMatches(final Class<?> type, final EclipseNode node, final TypeReference typeRef) {
		if (typeRef == null || typeRef.getTypeName() == null || typeRef.getTypeName().length == 0) return false;
		final String lastPartA = new String(typeRef.getTypeName()[typeRef.getTypeName().length - 1]);
		final String lastPartB = type.getSimpleName();
		if (!lastPartA.equals(lastPartB)) return false;
		final String typeName = toQualifiedName(typeRef.getTypeName());
		
		final TypeResolver resolver = new TypeResolver(node.getImportList());
		return resolver.typeMatches(node, type.getName(), typeName);
		
	}
	
	public static void sanityCheckForMethodGeneratingAnnotationsOnBuilderClass(final EclipseNode typeNode, final EclipseNode errorNode) {
		List<String> disallowed = null;
		for (final EclipseNode child : typeNode.down()) {
			if (child.getKind() != Kind.ANNOTATION) continue;
			for (final Class<? extends java.lang.annotation.Annotation> annType : INVALID_ON_BUILDERS) {
				if (annotationTypeMatches(annType, child)) {
					if (disallowed == null) disallowed = new ArrayList<String>();
					disallowed.add(annType.getSimpleName());
				}
			}
		}
		
		final int size = disallowed == null ? 0 : disallowed.size();
		if (size == 0) return;
		if (size == 1) {
			errorNode.addError("@" + disallowed.get(0) + " is not allowed on builder classes.");
			return;
		}
		final StringBuilder out = new StringBuilder();
		for (final String a : disallowed)
			out.append("@").append(a).append(", ");
		out.setLength(out.length() - 2);
		errorNode.addError(out.append(" are not allowed on builder classes.").toString());
	}
	
	public static Annotation copyAnnotation(final Annotation annotation, final ASTNode source) {
		final int pS = source.sourceStart, pE = source.sourceEnd;
		
		if (annotation instanceof MarkerAnnotation) {
			final MarkerAnnotation ann = new MarkerAnnotation(copyType(annotation.type, source), pS);
			setGeneratedBy(ann, source);
			ann.declarationSourceEnd = ann.sourceEnd = ann.statementEnd = pE;
			return ann;
		}
		
		if (annotation instanceof SingleMemberAnnotation) {
			final SingleMemberAnnotation ann = new SingleMemberAnnotation(copyType(annotation.type, source), pS);
			setGeneratedBy(ann, source);
			ann.declarationSourceEnd = ann.sourceEnd = ann.statementEnd = pE;
			// TODO memberValue(s) need to be copied as well (same for copying a
			// NormalAnnotation as below).
			ann.memberValue = ((SingleMemberAnnotation) annotation).memberValue;
			return ann;
		}
		
		if (annotation instanceof NormalAnnotation) {
			final NormalAnnotation ann = new NormalAnnotation(copyType(annotation.type, source), pS);
			setGeneratedBy(ann, source);
			ann.declarationSourceEnd = ann.statementEnd = ann.sourceEnd = pE;
			ann.memberValuePairs = ((NormalAnnotation) annotation).memberValuePairs;
			return ann;
		}
		
		return annotation;
	}
	
	/**
	 * You can't share TypeParameter objects or bad things happen; for example,
	 * one 'T' resolves differently from another 'T', even for the same T in a
	 * single class file. Unfortunately the TypeParameter type hierarchy is
	 * complicated and there's no clone method on TypeParameter itself. This
	 * method can clone them.
	 */
	public static TypeParameter[] copyTypeParams(final TypeParameter[] params, final ASTNode source) {
		if (params == null) return null;
		final TypeParameter[] out = new TypeParameter[params.length];
		int idx = 0;
		for (final TypeParameter param : params) {
			final TypeParameter o = new TypeParameter();
			setGeneratedBy(o, source);
			o.annotations = param.annotations;
			o.bits = param.bits;
			o.modifiers = param.modifiers;
			o.name = param.name;
			o.type = copyType(param.type, source);
			o.sourceStart = param.sourceStart;
			o.sourceEnd = param.sourceEnd;
			o.declarationEnd = param.declarationEnd;
			o.declarationSourceStart = param.declarationSourceStart;
			o.declarationSourceEnd = param.declarationSourceEnd;
			if (param.bounds != null) {
				final TypeReference[] b = new TypeReference[param.bounds.length];
				int idx2 = 0;
				for (final TypeReference ref : param.bounds)
					b[idx2++] = copyType(ref, source);
				o.bounds = b;
			}
			out[idx++] = o;
		}
		return out;
	}
	
	public static TypeReference namePlusTypeParamsToTypeReference(final char[] typeName, final TypeParameter[] params, final long p) {
		if (params != null && params.length > 0) {
			final TypeReference[] refs = new TypeReference[params.length];
			int idx = 0;
			for (final TypeParameter param : params) {
				final TypeReference typeRef = new SingleTypeReference(param.name, p);
				refs[idx++] = typeRef;
			}
			return new ParameterizedSingleTypeReference(typeName, refs, 0, p);
		}
		
		return new SingleTypeReference(typeName, p);
	}
	
	public static TypeReference[] copyTypes(final TypeReference[] refs) {
		return copyTypes(refs, null);
	}
	
	/**
	 * Convenience method that creates a new array and copies each TypeReference
	 * in the source array via {@link #copyType(TypeReference, ASTNode)}.
	 */
	public static TypeReference[] copyTypes(final TypeReference[] refs, final ASTNode source) {
		if (refs == null) return null;
		final TypeReference[] outs = new TypeReference[refs.length];
		int idx = 0;
		for (final TypeReference ref : refs) {
			outs[idx++] = copyType(ref, source);
		}
		return outs;
	}
	
	public static TypeReference copyType(final TypeReference ref) {
		return copyType(ref, null);
	}
	
	/**
	 * You can't share TypeReference objects or subtle errors start happening.
	 * Unfortunately the TypeReference type hierarchy is complicated and there's
	 * no clone method on TypeReference itself. This method can clone them.
	 */
	public static TypeReference copyType(final TypeReference ref, final ASTNode source) {
		if (ref instanceof ParameterizedQualifiedTypeReference) {
			final ParameterizedQualifiedTypeReference iRef = (ParameterizedQualifiedTypeReference) ref;
			TypeReference[][] args = null;
			if (iRef.typeArguments != null) {
				args = new TypeReference[iRef.typeArguments.length][];
				int idx = 0;
				for (final TypeReference[] inRefArray : iRef.typeArguments) {
					if (inRefArray == null) args[idx++] = null;
					else {
						final TypeReference[] outRefArray = new TypeReference[inRefArray.length];
						int idx2 = 0;
						for (final TypeReference inRef : inRefArray) {
							outRefArray[idx2++] = copyType(inRef, source);
						}
						args[idx++] = outRefArray;
					}
				}
			}
			
			final TypeReference typeRef = new ParameterizedQualifiedTypeReference(iRef.tokens, args, iRef.dimensions(), copy(iRef.sourcePositions));
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		if (ref instanceof ArrayQualifiedTypeReference) {
			final ArrayQualifiedTypeReference iRef = (ArrayQualifiedTypeReference) ref;
			final TypeReference typeRef = new ArrayQualifiedTypeReference(iRef.tokens, iRef.dimensions(), copy(iRef.sourcePositions));
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		if (ref instanceof QualifiedTypeReference) {
			final QualifiedTypeReference iRef = (QualifiedTypeReference) ref;
			final TypeReference typeRef = new QualifiedTypeReference(iRef.tokens, copy(iRef.sourcePositions));
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		if (ref instanceof ParameterizedSingleTypeReference) {
			final ParameterizedSingleTypeReference iRef = (ParameterizedSingleTypeReference) ref;
			TypeReference[] args = null;
			if (iRef.typeArguments != null) {
				args = new TypeReference[iRef.typeArguments.length];
				int idx = 0;
				for (final TypeReference inRef : iRef.typeArguments) {
					if (inRef == null) args[idx++] = null;
					else
						args[idx++] = copyType(inRef, source);
				}
			}
			
			final TypeReference typeRef = new ParameterizedSingleTypeReference(iRef.token, args, iRef.dimensions(), (long) iRef.sourceStart << 32 | iRef.sourceEnd);
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		if (ref instanceof ArrayTypeReference) {
			final ArrayTypeReference iRef = (ArrayTypeReference) ref;
			final TypeReference typeRef = new ArrayTypeReference(iRef.token, iRef.dimensions(), (long) iRef.sourceStart << 32 | iRef.sourceEnd);
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		if (ref instanceof Wildcard) {
			final Wildcard original = (Wildcard) ref;
			
			final Wildcard wildcard = new Wildcard(original.kind);
			wildcard.sourceStart = original.sourceStart;
			wildcard.sourceEnd = original.sourceEnd;
			if (original.bound != null) wildcard.bound = copyType(original.bound, source);
			if (source != null) setGeneratedBy(wildcard, source);
			return wildcard;
		}
		
		if (ref instanceof SingleTypeReference) {
			final SingleTypeReference iRef = (SingleTypeReference) ref;
			final TypeReference typeRef = new SingleTypeReference(iRef.token, (long) iRef.sourceStart << 32 | iRef.sourceEnd);
			if (source != null) setGeneratedBy(typeRef, source);
			return typeRef;
		}
		
		return ref;
	}
	
	public static Annotation[] copyAnnotations(final ASTNode source, final Annotation[]... allAnnotations) {
		List<Annotation> result = null;
		for (final Annotation[] annotations : allAnnotations) {
			if (annotations != null) {
				for (final Annotation annotation : annotations) {
					if (result == null) result = new ArrayList<Annotation>();
					result.add(copyAnnotation(annotation, source));
				}
			}
		}
		
		return result == null ? null : result.toArray(new Annotation[0]);
	}
	
	public static boolean hasAnnotation(final Class<? extends java.lang.annotation.Annotation> type, final EclipseNode node) {
		if (node == null) return false;
		if (type == null) return false;
		switch (node.getKind()) {
		case ARGUMENT:
		case FIELD:
		case LOCAL:
		case TYPE:
		case METHOD:
			for (final EclipseNode child : node.down()) {
				if (annotationTypeMatches(type, child)) return true;
			}
			// intentional fallthrough
		default:
			return false;
		}
	}
	
	/**
	 * Checks if the provided annotation type is likely to be the intended type
	 * for the given annotation node.
	 * 
	 * This is a guess, but a decent one.
	 */
	public static boolean annotationTypeMatches(final Class<? extends java.lang.annotation.Annotation> type, final EclipseNode node) {
		if (node.getKind() != Kind.ANNOTATION) return false;
		return typeMatches(type, node, ((Annotation) node.get()).type);
	}
	
	public static TypeReference cloneSelfType(final EclipseNode context) {
		return cloneSelfType(context, null);
	}
	
	public static TypeReference cloneSelfType(final EclipseNode context, final ASTNode source) {
		final int pS = source == null ? 0 : source.sourceStart, pE = source == null ? 0 : source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		EclipseNode type = context;
		TypeReference result = null;
		while (type != null && type.getKind() != Kind.TYPE)
			type = type.up();
		if (type != null && type.get() instanceof TypeDeclaration) {
			final TypeDeclaration typeDecl = (TypeDeclaration) type.get();
			if (typeDecl.typeParameters != null && typeDecl.typeParameters.length > 0) {
				final TypeReference[] refs = new TypeReference[typeDecl.typeParameters.length];
				int idx = 0;
				for (final TypeParameter param : typeDecl.typeParameters) {
					final TypeReference typeRef = new SingleTypeReference(param.name, (long) param.sourceStart << 32 | param.sourceEnd);
					if (source != null) setGeneratedBy(typeRef, source);
					refs[idx++] = typeRef;
				}
				result = new ParameterizedSingleTypeReference(typeDecl.name, refs, 0, p);
			} else {
				result = new SingleTypeReference(((TypeDeclaration) type.get()).name, p);
			}
		}
		if (result != null && source != null) setGeneratedBy(result, source);
		return result;
	}
	
	public static TypeReference makeType(TypeBinding binding, final ASTNode pos, final boolean allowCompound) {
		final int dims = binding.dimensions();
		binding = binding.leafComponentType();
		
		// Primitives
		
		char[] base = null;
		
		switch (binding.id) {
		case TypeIds.T_int:
			base = TypeConstants.INT;
			break;
		case TypeIds.T_long:
			base = TypeConstants.LONG;
			break;
		case TypeIds.T_short:
			base = TypeConstants.SHORT;
			break;
		case TypeIds.T_byte:
			base = TypeConstants.BYTE;
			break;
		case TypeIds.T_double:
			base = TypeConstants.DOUBLE;
			break;
		case TypeIds.T_float:
			base = TypeConstants.FLOAT;
			break;
		case TypeIds.T_boolean:
			base = TypeConstants.BOOLEAN;
			break;
		case TypeIds.T_char:
			base = TypeConstants.CHAR;
			break;
		case TypeIds.T_void:
			base = TypeConstants.VOID;
			break;
		case TypeIds.T_null:
			return null;
		}
		
		if (base != null) {
			if (dims > 0) {
				final TypeReference result = new ArrayTypeReference(base, dims, pos(pos));
				setGeneratedBy(result, pos);
				return result;
			}
			final TypeReference result = new SingleTypeReference(base, pos(pos));
			setGeneratedBy(result, pos);
			return result;
		}
		
		if (binding.isAnonymousType()) {
			final ReferenceBinding ref = (ReferenceBinding) binding;
			ReferenceBinding[] supers = ref.superInterfaces();
			if (supers == null || supers.length == 0) supers = new ReferenceBinding[] {ref.superclass()};
			if (supers[0] == null) {
				final TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				setGeneratedBy(result, pos);
				return result;
			}
			return makeType(supers[0], pos, false);
		}
		
		if (binding instanceof CaptureBinding) {
			return makeType(((CaptureBinding) binding).wildcard, pos, allowCompound);
		}
		
		if (binding.isUnboundWildcard()) {
			if (!allowCompound) {
				final TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				setGeneratedBy(result, pos);
				return result;
			} else {
				final Wildcard out = new Wildcard(Wildcard.UNBOUND);
				setGeneratedBy(out, pos);
				out.sourceStart = pos.sourceStart;
				out.sourceEnd = pos.sourceEnd;
				return out;
			}
		}
		
		if (binding.isWildcard()) {
			final WildcardBinding wildcard = (WildcardBinding) binding;
			if (wildcard.boundKind == Wildcard.EXTENDS) {
				if (!allowCompound) {
					return makeType(wildcard.bound, pos, false);
				} else {
					final Wildcard out = new Wildcard(Wildcard.EXTENDS);
					setGeneratedBy(out, pos);
					out.bound = makeType(wildcard.bound, pos, false);
					out.sourceStart = pos.sourceStart;
					out.sourceEnd = pos.sourceEnd;
					return out;
				}
			} else if (allowCompound && wildcard.boundKind == Wildcard.SUPER) {
				final Wildcard out = new Wildcard(Wildcard.SUPER);
				setGeneratedBy(out, pos);
				out.bound = makeType(wildcard.bound, pos, false);
				out.sourceStart = pos.sourceStart;
				out.sourceEnd = pos.sourceEnd;
				return out;
			} else {
				final TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				setGeneratedBy(result, pos);
				return result;
			}
		}
		
		// Keep moving up via 'binding.enclosingType()' and gather generics from
		// each binding. We stop after a local type, or a static type, or a
		// top-level type.
		// Finally, add however many nullTypeArgument[] arrays as that are
		// missing, inverse the list, toArray it, and use that as PTR's
		// typeArgument argument.
		
		final List<TypeReference[]> params = new ArrayList<TypeReference[]>();
		/* Calculate generics */{
			TypeBinding b = binding;
			while (true) {
				final boolean isFinalStop = b.isLocalType() || !b.isMemberType() || b.enclosingType() == null;
				
				TypeReference[] tyParams = null;
				if (b instanceof ParameterizedTypeBinding) {
					final ParameterizedTypeBinding paramized = (ParameterizedTypeBinding) b;
					if (paramized.arguments != null) {
						tyParams = new TypeReference[paramized.arguments.length];
						for (int i = 0; i < tyParams.length; i++) {
							tyParams[i] = makeType(paramized.arguments[i], pos, true);
						}
					}
				}
				
				params.add(tyParams);
				if (isFinalStop) break;
				b = b.enclosingType();
			}
		}
		
		char[][] parts;
		
		if (binding.isTypeVariable()) {
			parts = new char[][] {binding.shortReadableName()};
		} else if (binding.isLocalType()) {
			parts = new char[][] {binding.sourceName()};
		} else {
			String[] pkg = new String(binding.qualifiedPackageName()).split("\\.");
			final String[] name = new String(binding.qualifiedSourceName()).split("\\.");
			if (pkg.length == 1 && pkg[0].isEmpty()) pkg = new String[0];
			parts = new char[pkg.length + name.length][];
			int ptr;
			for (ptr = 0; ptr < pkg.length; ptr++)
				parts[ptr] = pkg[ptr].toCharArray();
			for (; ptr < pkg.length + name.length; ptr++)
				parts[ptr] = name[ptr - pkg.length].toCharArray();
		}
		
		while (params.size() < parts.length)
			params.add(null);
		Collections.reverse(params);
		
		boolean isParamized = false;
		
		for (final TypeReference[] tyParams : params) {
			if (tyParams != null) {
				isParamized = true;
				break;
			}
		}
		if (isParamized) {
			if (parts.length > 1) {
				final TypeReference[][] typeArguments = params.toArray(new TypeReference[0][]);
				final TypeReference result = new ParameterizedQualifiedTypeReference(parts, typeArguments, dims, poss(pos, parts.length));
				setGeneratedBy(result, pos);
				return result;
			}
			final TypeReference result = new ParameterizedSingleTypeReference(parts[0], params.get(0), dims, pos(pos));
			setGeneratedBy(result, pos);
			return result;
		}
		
		if (dims > 0) {
			if (parts.length > 1) {
				final TypeReference result = new ArrayQualifiedTypeReference(parts, dims, poss(pos, parts.length));
				setGeneratedBy(result, pos);
				return result;
			}
			final TypeReference result = new ArrayTypeReference(parts[0], dims, pos(pos));
			setGeneratedBy(result, pos);
			return result;
		}
		
		if (parts.length > 1) {
			final TypeReference result = new QualifiedTypeReference(parts, poss(pos, parts.length));
			setGeneratedBy(result, pos);
			return result;
		}
		final TypeReference result = new SingleTypeReference(parts[0], pos(pos));
		setGeneratedBy(result, pos);
		return result;
	}
	
	/**
	 * Provides AnnotationValues with the data it needs to do its thing.
	 */
	public static <A extends java.lang.annotation.Annotation> AnnotationValues<A> createAnnotation(final Class<A> type, final EclipseNode annotationNode) {
		
		final Annotation annotation = (Annotation) annotationNode.get();
		final Map<String, AnnotationValue> values = new HashMap<String, AnnotationValue>();
		
		final MemberValuePair[] memberValuePairs = annotation.memberValuePairs();
		
		if (memberValuePairs != null) for (final MemberValuePair pair : memberValuePairs) {
			final List<String> raws = new ArrayList<String>();
			final List<Object> expressionValues = new ArrayList<Object>();
			final List<Object> guesses = new ArrayList<Object>();
			Expression[] expressions = null;
			
			final char[] n = pair.name;
			final String mName = (n == null || n.length == 0) ? "value" : new String(pair.name);
			final Expression rhs = pair.value;
			if (rhs instanceof ArrayInitializer) {
				expressions = ((ArrayInitializer) rhs).expressions;
			} else if (rhs != null) {
				expressions = new Expression[] {rhs};
			}
			if (expressions != null) for (final Expression ex : expressions) {
				final StringBuffer sb = new StringBuffer();
				ex.print(0, sb);
				raws.add(sb.toString());
				expressionValues.add(ex);
				guesses.add(calculateValue(ex));
			}
			
			final Expression[] exprs = expressions;
			values.put(mName, new AnnotationValue(annotationNode, raws, expressionValues, guesses, true) {
				@Override public void setError(final String message, final int valueIdx) {
					Expression ex;
					if (valueIdx == -1) ex = rhs;
					else
						ex = exprs != null ? exprs[valueIdx] : null;
					
					if (ex == null) ex = annotation;
					
					final int sourceStart = ex.sourceStart;
					final int sourceEnd = ex.sourceEnd;
					
					annotationNode.addError(message, sourceStart, sourceEnd);
				}
				
				@Override public void setWarning(final String message, final int valueIdx) {
					Expression ex;
					if (valueIdx == -1) ex = rhs;
					else
						ex = exprs != null ? exprs[valueIdx] : null;
					
					if (ex == null) ex = annotation;
					
					final int sourceStart = ex.sourceStart;
					final int sourceEnd = ex.sourceEnd;
					
					annotationNode.addWarning(message, sourceStart, sourceEnd);
				}
			});
		}
		
		for (final Method m : type.getDeclaredMethods()) {
			if (!Modifier.isPublic(m.getModifiers())) continue;
			final String name = m.getName();
			if (!values.containsKey(name)) {
				values.put(name, new AnnotationValue(annotationNode, new ArrayList<String>(), new ArrayList<Object>(), new ArrayList<Object>(), false) {
					@Override public void setError(final String message, final int valueIdx) {
						annotationNode.addError(message);
					}
					
					@Override public void setWarning(final String message, final int valueIdx) {
						annotationNode.addWarning(message);
					}
				});
			}
		}
		
		return new AnnotationValues<A>(type, values, annotationNode);
	}
	
	/**
	 * Turns an {@code AccessLevel} instance into the flag bit used by eclipse.
	 */
	public static int toEclipseModifier(final AccessLevel value) {
		switch (value) {
		case MODULE:
		case PACKAGE:
			return 0;
		default:
		case PUBLIC:
			return ClassFileConstants.AccPublic;
		case PROTECTED:
			return ClassFileConstants.AccProtected;
		case NONE:
		case PRIVATE:
			return ClassFileConstants.AccPrivate;
		}
	}
	
	private static class GetterMethod {
		private final char[] name;
		private final TypeReference type;
		
		GetterMethod(final char[] name, final TypeReference type) {
			this.name = name;
			this.type = type;
		}
	}
	
	static void registerCreatedLazyGetter(final FieldDeclaration field, final char[] methodName, final TypeReference returnType) {
		if (isBoolean(returnType)) {
			FieldDeclaration_booleanLazyGetter.set(field, true);
		}
	}
	
	public static boolean isBoolean(final TypeReference typeReference) {
		return nameEquals(typeReference.getTypeName(), "boolean") && typeReference.dimensions() == 0;
	}
	
	private static GetterMethod findGetter(final EclipseNode field) {
		final FieldDeclaration fieldDeclaration = (FieldDeclaration) field.get();
		final boolean forceBool = FieldDeclaration_booleanLazyGetter.get(fieldDeclaration);
		final TypeReference fieldType = fieldDeclaration.type;
		final boolean isBoolean = forceBool || isBoolean(fieldType);
		
		final EclipseNode typeNode = field.up();
		for (final String potentialGetterName : toAllGetterNames(field, isBoolean)) {
			for (final EclipseNode potentialGetter : typeNode.down()) {
				if (potentialGetter.getKind() != Kind.METHOD) continue;
				if (!(potentialGetter.get() instanceof MethodDeclaration)) continue;
				final MethodDeclaration method = (MethodDeclaration) potentialGetter.get();
				if (!potentialGetterName.equalsIgnoreCase(new String(method.selector))) continue;
				/** static getX() methods don't count. */
				if ((method.modifiers & ClassFileConstants.AccStatic) != 0) continue;
				/** Nor do getters with a non-empty parameter list. */
				if (method.arguments != null && method.arguments.length > 0) continue;
				return new GetterMethod(method.selector, method.returnType);
			}
		}
		
		// Check if the field has a @Getter annotation.
		
		boolean hasGetterAnnotation = false;
		
		for (final EclipseNode child : field.down()) {
			if (child.getKind() == Kind.ANNOTATION && annotationTypeMatches(Getter.class, child)) {
				final AnnotationValues<Getter> ann = createAnnotation(Getter.class, child);
				if (ann.getInstance().value() == AccessLevel.NONE) return null; // Definitely
																				// WONT
																				// have
																				// a
																				// getter.
				hasGetterAnnotation = true;
			}
		}
		
		// Check if the class has a @Getter annotation.
		
		if (!hasGetterAnnotation && new HandleGetter().fieldQualifiesForGetterGeneration(field)) {
			// Check if the class has @Getter or @Data annotation.
			
			final EclipseNode containingType = field.up();
			if (containingType != null) for (final EclipseNode child : containingType.down()) {
				if (child.getKind() == Kind.ANNOTATION && annotationTypeMatches(Data.class, child)) hasGetterAnnotation = true;
				if (child.getKind() == Kind.ANNOTATION && annotationTypeMatches(Getter.class, child)) {
					final AnnotationValues<Getter> ann = createAnnotation(Getter.class, child);
					if (ann.getInstance().value() == AccessLevel.NONE) return null; // Definitely
																					// WONT
																					// have
																					// a
																					// getter.
					hasGetterAnnotation = true;
				}
			}
		}
		
		if (hasGetterAnnotation) {
			final String getterName = toGetterName(field, isBoolean);
			if (getterName == null) return null;
			return new GetterMethod(getterName.toCharArray(), fieldType);
		}
		
		return null;
	}
	
	public enum FieldAccess {
		GETTER, PREFER_FIELD, ALWAYS_FIELD;
	}
	
	static boolean lookForGetter(final EclipseNode field, final FieldAccess fieldAccess) {
		if (fieldAccess == FieldAccess.GETTER) return true;
		if (fieldAccess == FieldAccess.ALWAYS_FIELD) return false;
		
		// If @Getter(lazy = true) is used, then using it is mandatory.
		for (final EclipseNode child : field.down()) {
			if (child.getKind() != Kind.ANNOTATION) continue;
			if (annotationTypeMatches(Getter.class, child)) {
				final AnnotationValues<Getter> ann = createAnnotation(Getter.class, child);
				if (ann.getInstance().lazy()) return true;
			}
		}
		return false;
	}
	
	static TypeReference getFieldType(final EclipseNode field, final FieldAccess fieldAccess) {
		final boolean lookForGetter = lookForGetter(field, fieldAccess);
		
		final GetterMethod getter = lookForGetter ? findGetter(field) : null;
		if (getter == null) {
			return ((FieldDeclaration) field.get()).type;
		}
		
		return getter.type;
	}
	
	static Expression createFieldAccessor(final EclipseNode field, final FieldAccess fieldAccess, final ASTNode source) {
		final int pS = source == null ? 0 : source.sourceStart, pE = source == null ? 0 : source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		
		final boolean lookForGetter = lookForGetter(field, fieldAccess);
		
		final GetterMethod getter = lookForGetter ? findGetter(field) : null;
		
		if (getter == null) {
			final FieldDeclaration fieldDecl = (FieldDeclaration) field.get();
			final FieldReference ref = new FieldReference(fieldDecl.name, p);
			if ((fieldDecl.modifiers & ClassFileConstants.AccStatic) != 0) {
				final EclipseNode containerNode = field.up();
				if (containerNode != null && containerNode.get() instanceof TypeDeclaration) {
					ref.receiver = new SingleNameReference(((TypeDeclaration) containerNode.get()).name, p);
				} else {
					final Expression smallRef = new FieldReference(field.getName().toCharArray(), p);
					if (source != null) setGeneratedBy(smallRef, source);
					return smallRef;
				}
			} else {
				ref.receiver = new ThisReference(pS, pE);
			}
			
			if (source != null) {
				setGeneratedBy(ref, source);
				setGeneratedBy(ref.receiver, source);
			}
			return ref;
		}
		
		final MessageSend call = new MessageSend();
		setGeneratedBy(call, source);
		call.sourceStart = pS;
		call.statementEnd = call.sourceEnd = pE;
		call.receiver = new ThisReference(pS, pE);
		setGeneratedBy(call.receiver, source);
		call.selector = getter.name;
		return call;
	}
	
	static Expression createFieldAccessor(final EclipseNode field, final FieldAccess fieldAccess, final ASTNode source, final char[] receiver) {
		final int pS = source.sourceStart, pE = source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		
		final boolean lookForGetter = lookForGetter(field, fieldAccess);
		
		final GetterMethod getter = lookForGetter ? findGetter(field) : null;
		
		if (getter == null) {
			NameReference ref;
			
			final char[][] tokens = new char[2][];
			tokens[0] = receiver;
			tokens[1] = field.getName().toCharArray();
			final long[] poss = {p, p};
			
			ref = new QualifiedNameReference(tokens, poss, pS, pE);
			setGeneratedBy(ref, source);
			return ref;
		}
		
		final MessageSend call = new MessageSend();
		setGeneratedBy(call, source);
		call.sourceStart = pS;
		call.statementEnd = call.sourceEnd = pE;
		call.receiver = new SingleNameReference(receiver, p);
		setGeneratedBy(call.receiver, source);
		call.selector = getter.name;
		return call;
	}
	
	/**
	 * Serves as return value for the methods that check for the existence of
	 * fields and methods.
	 */
	public enum MemberExistsResult {
		NOT_EXISTS, EXISTS_BY_LOMBOK, EXISTS_BY_USER;
	}
	
	/**
	 * Translates the given field into all possible getter names. Convenient
	 * wrapper around
	 * {@link TransformationsUtil#toAllGetterNames(lombok.core.AnnotationValues, CharSequence, boolean)}
	 * .
	 */
	public static List<String> toAllGetterNames(final EclipseNode field, final boolean isBoolean) {
		return HandlerUtil.toAllGetterNames(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * @return the likely getter name for the stated field. (e.g. private
	 *         boolean foo; to isFoo).
	 * 
	 *         Convenient wrapper around
	 *         {@link TransformationsUtil#toGetterName(lombok.core.AnnotationValues, CharSequence, boolean)}
	 *         .
	 */
	public static String toGetterName(final EclipseNode field, final boolean isBoolean) {
		
		final String fileName = field.getAst().getFileName();
		if (fileName.equals("domain/models/fileshare/vo/createuser/CreateUser.java")) {
			FileLog.log("-- called toGetterName(EclipseNode , boolean)  //field.getAst.filename: " + fileName);
		}
		return HandlerUtil.toGetterName(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * Translates the given field into all possible setter names. Convenient
	 * wrapper around
	 * {@link TransformationsUtil#toAllSetterNames(lombok.core.AnnotationValues, CharSequence, boolean)}
	 * .
	 */
	public static java.util.List<String> toAllSetterNames(final EclipseNode field, final boolean isBoolean) {
		return HandlerUtil.toAllSetterNames(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * @return the likely setter name for the stated field. (e.g. private
	 *         boolean foo; to setFoo).
	 * 
	 *         Convenient wrapper around
	 *         {@link TransformationsUtil#toSetterName(lombok.core.AnnotationValues, CharSequence, boolean)}
	 *         .
	 */
	public static String toSetterName(final EclipseNode field, final boolean isBoolean) {
		return HandlerUtil.toSetterName(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * Translates the given field into all possible wither names. Convenient
	 * wrapper around
	 * {@link TransformationsUtil#toAllWitherNames(lombok.core.AnnotationValues, CharSequence, boolean)}
	 * .
	 */
	public static java.util.List<String> toAllWitherNames(final EclipseNode field, final boolean isBoolean) {
		return HandlerUtil.toAllWitherNames(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * @return the likely wither name for the stated field. (e.g. private
	 *         boolean foo; to withFoo).
	 * 
	 *         Convenient wrapper around
	 *         {@link TransformationsUtil#toWitherName(lombok.core.AnnotationValues, CharSequence, boolean)}
	 *         .
	 */
	public static String toWitherName(final EclipseNode field, final boolean isBoolean) {
		return HandlerUtil.toWitherName(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean);
	}
	
	/**
	 * When generating a setter, the setter either returns void (beanspec) or
	 * Self (fluent). This method scans for the {@code Accessors} annotation and
	 * associated config properties to figure that out.
	 */
	public static boolean shouldReturnThis(final EclipseNode field) {
		if ((((FieldDeclaration) field.get()).modifiers & ClassFileConstants.AccStatic) != 0) return false;
		final AnnotationValues<Accessors> accessors = EclipseHandlerUtil.getAccessorsForField(field);
		return shouldReturnThis0(accessors, field.getAst());
	}
	
	/**
	 * Checks if the field should be included in operations that work on 'all'
	 * fields: If the field is static, or starts with a '$', or is actually an
	 * enum constant, 'false' is returned, indicating you should skip it.
	 */
	public static boolean filterField(final FieldDeclaration declaration) {
		return filterField(declaration, true);
	}
	
	public static boolean filterField(final FieldDeclaration declaration, final boolean skipStatic) {
		// Skip the fake fields that represent enum constants.
		if (declaration.initialization instanceof AllocationExpression && ((AllocationExpression) declaration.initialization).enumConstant != null) return false;
		
		if (declaration.type == null) return false;
		
		// Skip fields that start with $
		if (declaration.name.length > 0 && declaration.name[0] == '$') return false;
		
		// Skip static fields.
		if (skipStatic && (declaration.modifiers & ClassFileConstants.AccStatic) != 0) return false;
		
		return true;
	}
	
	public static char[] removePrefixFromField(final EclipseNode field) {
		List<String> prefixes = null;
		for (final EclipseNode node : field.down()) {
			if (annotationTypeMatches(Accessors.class, node)) {
				final AnnotationValues<Accessors> ann = createAnnotation(Accessors.class, node);
				if (ann.isExplicit("prefix")) prefixes = Arrays.asList(ann.getInstance().prefix());
				break;
			}
		}
		
		if (prefixes == null) {
			EclipseNode current = field.up();
			outer: while (current != null) {
				for (final EclipseNode node : current.down()) {
					if (annotationTypeMatches(Accessors.class, node)) {
						final AnnotationValues<Accessors> ann = createAnnotation(Accessors.class, node);
						if (ann.isExplicit("prefix")) prefixes = Arrays.asList(ann.getInstance().prefix());
						break outer;
					}
				}
				current = current.up();
			}
		}
		
		if (prefixes == null) prefixes = field.getAst().readConfiguration(ConfigurationKeys.ACCESSORS_PREFIX);
		if (!prefixes.isEmpty()) {
			final CharSequence newName = removePrefix(field.getName(), prefixes);
			if (newName != null) return newName.toString().toCharArray();
		}
		
		return ((FieldDeclaration) field.get()).name;
	}
	
	public static AnnotationValues<Accessors> getAccessorsForField(final EclipseNode field) {
		for (final EclipseNode node : field.down()) {
			if (annotationTypeMatches(Accessors.class, node)) {
				return createAnnotation(Accessors.class, node);
			}
		}
		
		EclipseNode current = field.up();
		while (current != null) {
			for (final EclipseNode node : current.down()) {
				if (annotationTypeMatches(Accessors.class, node)) {
					return createAnnotation(Accessors.class, node);
				}
			}
			current = current.up();
		}
		
		return AnnotationValues.of(Accessors.class, field);
	}
	
	/**
	 * Checks if there is a field with the provided name.
	 * 
	 * @param fieldName
	 *            the field name to check for.
	 * @param node
	 *            Any node that represents the Type (TypeDeclaration) to look
	 *            in, or any child node thereof.
	 */
	public static MemberExistsResult fieldExists(final String fieldName, EclipseNode node) {
		while (node != null && !(node.get() instanceof TypeDeclaration)) {
			node = node.up();
		}
		
		if (node != null && node.get() instanceof TypeDeclaration) {
			final TypeDeclaration typeDecl = (TypeDeclaration) node.get();
			if (typeDecl.fields != null) for (final FieldDeclaration def : typeDecl.fields) {
				final char[] fName = def.name;
				if (fName == null) continue;
				if (fieldName.equals(new String(fName))) {
					return getGeneratedBy(def) == null ? MemberExistsResult.EXISTS_BY_USER : MemberExistsResult.EXISTS_BY_LOMBOK;
				}
			}
		}
		
		return MemberExistsResult.NOT_EXISTS;
	}
	
	/**
	 * Wrapper for {@link #methodExists(String, EclipseNode, boolean, int)} with
	 * {@code caseSensitive} = {@code true}.
	 */
	public static MemberExistsResult methodExists(final String methodName, final EclipseNode node, final int params) {
		return methodExists(methodName, node, true, params);
	}
	
	/**
	 * Checks if there is a method with the provided name. In case of multiple
	 * methods (overloading), only the first method decides if EXISTS_BY_USER or
	 * EXISTS_BY_LOMBOK is returned.
	 * 
	 * @param methodName
	 *            the method name to check for.
	 * @param node
	 *            Any node that represents the Type (TypeDeclaration) to look
	 *            in, or any child node thereof.
	 * @param caseSensitive
	 *            If the search should be case sensitive.
	 * @param params
	 *            The number of parameters the method should have; varargs count
	 *            as 0-*. Set to -1 to find any method with the appropriate name
	 *            regardless of parameter count.
	 */
	public static MemberExistsResult methodExists(final String methodName, EclipseNode node, final boolean caseSensitive, final int params) {
		while (node != null && !(node.get() instanceof TypeDeclaration)) {
			node = node.up();
		}
		
		if (node != null && node.get() instanceof TypeDeclaration) {
			final TypeDeclaration typeDecl = (TypeDeclaration) node.get();
			if (typeDecl.methods != null) top: for (final AbstractMethodDeclaration def : typeDecl.methods) {
				if (def instanceof MethodDeclaration) {
					final char[] mName = def.selector;
					if (mName == null) continue;
					final boolean nameEquals = caseSensitive ? methodName.equals(new String(mName)) : methodName.equalsIgnoreCase(new String(mName));
					if (nameEquals) {
						if (params > -1) {
							int minArgs = 0;
							int maxArgs = 0;
							if (def.arguments != null && def.arguments.length > 0) {
								minArgs = def.arguments.length;
								if ((def.arguments[def.arguments.length - 1].type.bits & ASTNode.IsVarArgs) != 0) {
									minArgs--;
									maxArgs = Integer.MAX_VALUE;
								} else {
									maxArgs = minArgs;
								}
							}
							
							if (params < minArgs || params > maxArgs) continue;
						}
						
						if (def.annotations != null) for (final Annotation anno : def.annotations) {
							if (typeMatches(Tolerate.class, node, anno.type)) continue top;
						}
						
						return getGeneratedBy(def) == null ? MemberExistsResult.EXISTS_BY_USER : MemberExistsResult.EXISTS_BY_LOMBOK;
					}
				}
			}
		}
		
		return MemberExistsResult.NOT_EXISTS;
	}
	
	/**
	 * Checks if there is a (non-default) constructor. In case of multiple
	 * constructors (overloading), only the first constructor decides if
	 * EXISTS_BY_USER or EXISTS_BY_LOMBOK is returned.
	 * 
	 * @param node
	 *            Any node that represents the Type (TypeDeclaration) to look
	 *            in, or any child node thereof.
	 */
	public static MemberExistsResult constructorExists(EclipseNode node) {
		while (node != null && !(node.get() instanceof TypeDeclaration)) {
			node = node.up();
		}
		
		if (node != null && node.get() instanceof TypeDeclaration) {
			final TypeDeclaration typeDecl = (TypeDeclaration) node.get();
			if (typeDecl.methods != null) top: for (final AbstractMethodDeclaration def : typeDecl.methods) {
				if (def instanceof ConstructorDeclaration) {
					if ((def.bits & ASTNode.IsDefaultConstructor) != 0) continue;
					
					if (def.annotations != null) for (final Annotation anno : def.annotations) {
						if (typeMatches(Tolerate.class, node, anno.type)) continue top;
					}
					
					return getGeneratedBy(def) == null ? MemberExistsResult.EXISTS_BY_USER : MemberExistsResult.EXISTS_BY_LOMBOK;
				}
			}
		}
		
		return MemberExistsResult.NOT_EXISTS;
	}
	
	/**
	 * Inserts a field into an existing type. The type must represent a
	 * {@code TypeDeclaration}. The field carries the &#64;
	 * {@link SuppressWarnings}("all") annotation.
	 */
	public static EclipseNode injectFieldAndMarkGenerated(final EclipseNode type, final FieldDeclaration field) {
		field.annotations = addSuppressWarningsAll(type, field, field.annotations);
		field.annotations = addGenerated(type, field, field.annotations);
		return injectField(type, field);
	}
	
	/**
	 * Inserts a field into an existing type. The type must represent a
	 * {@code TypeDeclaration}.
	 */
	public static EclipseNode injectField(final EclipseNode type, final FieldDeclaration field) {
		final TypeDeclaration parent = (TypeDeclaration) type.get();
		
		if (parent.fields == null) {
			parent.fields = new FieldDeclaration[1];
			parent.fields[0] = field;
		} else {
			final int size = parent.fields.length;
			final FieldDeclaration[] newArray = new FieldDeclaration[size + 1];
			System.arraycopy(parent.fields, 0, newArray, 0, size);
			int index = 0;
			for (; index < size; index++) {
				final FieldDeclaration f = newArray[index];
				if (isEnumConstant(f) || isGenerated(f)) continue;
				break;
			}
			System.arraycopy(newArray, index, newArray, index + 1, size - index);
			newArray[index] = field;
			parent.fields = newArray;
		}
		
		if (isEnumConstant(field) || (field.modifiers & Modifier.STATIC) != 0) {
			if (!hasClinit(parent)) {
				parent.addClinit();
			}
		}
		
		return type.add(field, Kind.FIELD);
	}
	
	public static boolean isEnumConstant(final FieldDeclaration field) {
		return ((field.initialization instanceof AllocationExpression) && (((AllocationExpression) field.initialization).enumConstant == field));
	}
	
	/**
	 * Inserts a method into an existing type. The type must represent a
	 * {@code TypeDeclaration}.
	 */
	public static EclipseNode injectMethod(final EclipseNode type, final AbstractMethodDeclaration method) {
		method.annotations = addSuppressWarningsAll(type, method, method.annotations);
		method.annotations = addGenerated(type, method, method.annotations);
		final TypeDeclaration parent = (TypeDeclaration) type.get();
		
		if (parent.methods == null) {
			parent.methods = new AbstractMethodDeclaration[1];
			parent.methods[0] = method;
		} else {
			if (method instanceof ConstructorDeclaration) {
				for (int i = 0; i < parent.methods.length; i++) {
					if (parent.methods[i] instanceof ConstructorDeclaration && (parent.methods[i].bits & ASTNode.IsDefaultConstructor) != 0) {
						final EclipseNode tossMe = type.getNodeFor(parent.methods[i]);
						
						final AbstractMethodDeclaration[] withoutGeneratedConstructor = new AbstractMethodDeclaration[parent.methods.length - 1];
						
						System.arraycopy(parent.methods, 0, withoutGeneratedConstructor, 0, i);
						System.arraycopy(parent.methods, i + 1, withoutGeneratedConstructor, i, parent.methods.length - i - 1);
						
						parent.methods = withoutGeneratedConstructor;
						if (tossMe != null) tossMe.up().removeChild(tossMe);
						break;
					}
				}
			}
			// We insert the method in the last position of the methods
			// registered to the type
			// When changing this behavior, this may trigger issue #155 and #377
			final AbstractMethodDeclaration[] newArray = new AbstractMethodDeclaration[parent.methods.length + 1];
			System.arraycopy(parent.methods, 0, newArray, 0, parent.methods.length);
			newArray[parent.methods.length] = method;
			parent.methods = newArray;
		}
		
		return type.add(method, Kind.METHOD);
	}
	
	/**
	 * Adds an inner type (class, interface, enum) to the given type. Cannot
	 * inject top-level types.
	 * 
	 * @param typeNode
	 *            parent type to inject new type into
	 * @param type
	 *            New type (class, interface, etc) to inject.
	 */
	public static EclipseNode injectType(final EclipseNode typeNode, final TypeDeclaration type) {
		type.annotations = addSuppressWarningsAll(typeNode, type, type.annotations);
		type.annotations = addGenerated(typeNode, type, type.annotations);
		final TypeDeclaration parent = (TypeDeclaration) typeNode.get();
		
		if (parent.memberTypes == null) {
			parent.memberTypes = new TypeDeclaration[] {type};
		} else {
			final TypeDeclaration[] newArray = new TypeDeclaration[parent.memberTypes.length + 1];
			System.arraycopy(parent.memberTypes, 0, newArray, 0, parent.memberTypes.length);
			newArray[parent.memberTypes.length] = type;
			parent.memberTypes = newArray;
		}
		
		return typeNode.add(type, Kind.TYPE);
	}
	
	private static final char[] ALL = "all".toCharArray();
	private static final char[] JUSTIFICATION = "justification".toCharArray();
	private static final char[] GENERATED_CODE = "generated code".toCharArray();
	private static final char[] LOMBOK = "lombok".toCharArray();
	private static final char[][] JAVAX_ANNOTATION_GENERATED = Eclipse.fromQualifiedName("javax.annotation.Generated");
	private static final char[][] EDU_UMD_CS_FINDBUGS_ANNOTATIONS_SUPPRESSFBWARNINGS = Eclipse.fromQualifiedName("edu.umd.cs.findbugs.annotations.SuppressFBWarnings");
	
	public static Annotation[] addSuppressWarningsAll(final EclipseNode node, final ASTNode source, final Annotation[] originalAnnotationArray) {
		Annotation[] anns = addAnnotation(source, originalAnnotationArray, TypeConstants.JAVA_LANG_SUPPRESSWARNINGS, new StringLiteral(ALL, 0, 0, 0));
		
		if (Boolean.TRUE.equals(node.getAst().readConfiguration(ConfigurationKeys.ADD_FINDBUGS_SUPPRESSWARNINGS_ANNOTATIONS))) {
			final MemberValuePair mvp = new MemberValuePair(JUSTIFICATION, 0, 0, new StringLiteral(GENERATED_CODE, 0, 0, 0));
			anns = addAnnotation(source, anns, EDU_UMD_CS_FINDBUGS_ANNOTATIONS_SUPPRESSFBWARNINGS, mvp);
		}
		
		return anns;
	}
	
	public static Annotation[] addGenerated(final EclipseNode node, final ASTNode source, final Annotation[] originalAnnotationArray) {
		if (Boolean.FALSE.equals(node.getAst().readConfiguration(ConfigurationKeys.ADD_GENERATED_ANNOTATIONS))) return originalAnnotationArray;
		return addAnnotation(source, originalAnnotationArray, JAVAX_ANNOTATION_GENERATED, new StringLiteral(LOMBOK, 0, 0, 0));
	}
	
	private static Annotation[] addAnnotation(final ASTNode source, final Annotation[] originalAnnotationArray, final char[][] annotationTypeFqn, final ASTNode arg) {
		final char[] simpleName = annotationTypeFqn[annotationTypeFqn.length - 1];
		
		if (originalAnnotationArray != null) for (final Annotation ann : originalAnnotationArray) {
			char[] lastToken = null;
			
			if (ann.type instanceof QualifiedTypeReference) {
				final char[][] t = ((QualifiedTypeReference) ann.type).tokens;
				lastToken = t[t.length - 1];
			} else if (ann.type instanceof SingleTypeReference) {
				lastToken = ((SingleTypeReference) ann.type).token;
			}
			
			if (lastToken != null && Arrays.equals(simpleName, lastToken)) return originalAnnotationArray;
		}
		
		final int pS = source.sourceStart, pE = source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		final long[] poss = new long[annotationTypeFqn.length];
		Arrays.fill(poss, p);
		final QualifiedTypeReference qualifiedType = new QualifiedTypeReference(annotationTypeFqn, poss);
		setGeneratedBy(qualifiedType, source);
		Annotation ann;
		if (arg instanceof Expression) {
			final SingleMemberAnnotation sma = new SingleMemberAnnotation(qualifiedType, pS);
			sma.declarationSourceEnd = pE;
			arg.sourceStart = pS;
			arg.sourceEnd = pE;
			sma.memberValue = (Expression) arg;
			setGeneratedBy(sma.memberValue, source);
			ann = sma;
		} else if (arg instanceof MemberValuePair) {
			final NormalAnnotation na = new NormalAnnotation(qualifiedType, pS);
			na.declarationSourceEnd = pE;
			arg.sourceStart = pS;
			arg.sourceEnd = pE;
			na.memberValuePairs = new MemberValuePair[] {(MemberValuePair) arg};
			setGeneratedBy(na.memberValuePairs[0], source);
			setGeneratedBy(na.memberValuePairs[0].value, source);
			na.memberValuePairs[0].value.sourceStart = pS;
			na.memberValuePairs[0].value.sourceEnd = pE;
			ann = na;
		} else {
			final MarkerAnnotation ma = new MarkerAnnotation(qualifiedType, pS);
			ma.declarationSourceEnd = pE;
			ann = ma;
		}
		setGeneratedBy(ann, source);
		if (originalAnnotationArray == null) return new Annotation[] {ann};
		final Annotation[] newAnnotationArray = new Annotation[originalAnnotationArray.length + 1];
		System.arraycopy(originalAnnotationArray, 0, newAnnotationArray, 0, originalAnnotationArray.length);
		newAnnotationArray[originalAnnotationArray.length] = ann;
		return newAnnotationArray;
	}
	
	/**
	 * Generates a new statement that checks if the given variable is null, and
	 * if so, throws a specified exception with the variable name as message.
	 * 
	 * @param exName
	 *            The name of the exception to throw; normally
	 *            {@code java.lang.NullPointerException}.
	 */
	public static Statement generateNullCheck(final AbstractVariableDeclaration variable, final EclipseNode sourceNode) {
		NullCheckExceptionType exceptionType = sourceNode.getAst().readConfiguration(ConfigurationKeys.NON_NULL_EXCEPTION_TYPE);
		if (exceptionType == null) exceptionType = NullCheckExceptionType.NULL_POINTER_EXCEPTION;
		
		final ASTNode source = sourceNode.get();
		
		final int pS = source.sourceStart, pE = source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		
		if (isPrimitive(variable.type)) return null;
		final AllocationExpression exception = new AllocationExpression();
		setGeneratedBy(exception, source);
		int partCount = 1;
		final String exceptionTypeStr = exceptionType.getExceptionType();
		for (int i = 0; i < exceptionTypeStr.length(); i++)
			if (exceptionTypeStr.charAt(i) == '.') partCount++;
		final long[] ps = new long[partCount];
		Arrays.fill(ps, 0L);
		exception.type = new QualifiedTypeReference(fromQualifiedName(exceptionTypeStr), ps);
		setGeneratedBy(exception.type, source);
		exception.arguments = new Expression[] {new StringLiteral(exceptionType.toExceptionMessage(new String(variable.name)).toCharArray(), pS, pE, 0)};
		setGeneratedBy(exception.arguments[0], source);
		final ThrowStatement throwStatement = new ThrowStatement(exception, pS, pE);
		setGeneratedBy(throwStatement, source);
		
		final SingleNameReference varName = new SingleNameReference(variable.name, p);
		setGeneratedBy(varName, source);
		final NullLiteral nullLiteral = new NullLiteral(pS, pE);
		setGeneratedBy(nullLiteral, source);
		final EqualExpression equalExpression = new EqualExpression(varName, nullLiteral, OperatorIds.EQUAL_EQUAL);
		equalExpression.sourceStart = pS;
		equalExpression.statementEnd = equalExpression.sourceEnd = pE;
		setGeneratedBy(equalExpression, source);
		final Block throwBlock = new Block(0);
		throwBlock.statements = new Statement[] {throwStatement};
		throwBlock.sourceStart = pS;
		throwBlock.sourceEnd = pE;
		setGeneratedBy(throwBlock, source);
		final IfStatement ifStatement = new IfStatement(equalExpression, throwBlock, 0, 0);
		setGeneratedBy(ifStatement, source);
		return ifStatement;
	}
	
	/**
	 * Create an annotation of the given name, and is marked as being generated
	 * by the given source.
	 */
	public static MarkerAnnotation makeMarkerAnnotation(final char[][] name, final ASTNode source) {
		final long pos = (long) source.sourceStart << 32 | source.sourceEnd;
		final TypeReference typeRef = new QualifiedTypeReference(name, new long[] {pos, pos, pos});
		setGeneratedBy(typeRef, source);
		final MarkerAnnotation ann = new MarkerAnnotation(typeRef, (int) (pos >> 32));
		ann.declarationSourceEnd = ann.sourceEnd = ann.statementEnd = (int) pos;
		setGeneratedBy(ann, source);
		return ann;
	}
	
	/**
	 * Given a list of field names and a node referring to a type, finds each
	 * name in the list that does not match a field within the type.
	 */
	public static List<Integer> createListOfNonExistentFields(final List<String> list, final EclipseNode type, final boolean excludeStandard, final boolean excludeTransient) {
		final boolean[] matched = new boolean[list.size()];
		
		for (final EclipseNode child : type.down()) {
			if (list.isEmpty()) break;
			if (child.getKind() != Kind.FIELD) continue;
			if (excludeStandard) {
				if ((((FieldDeclaration) child.get()).modifiers & ClassFileConstants.AccStatic) != 0) continue;
				if (child.getName().startsWith("$")) continue;
			}
			if (excludeTransient && (((FieldDeclaration) child.get()).modifiers & ClassFileConstants.AccTransient) != 0) continue;
			final int idx = list.indexOf(child.getName());
			if (idx > -1) matched[idx] = true;
		}
		
		final List<Integer> problematic = new ArrayList<Integer>();
		for (int i = 0; i < list.size(); i++) {
			if (!matched[i]) problematic.add(i);
		}
		
		return problematic;
	}
	
	/**
	 * In eclipse 3.7+, the CastExpression constructor was changed from a really
	 * weird version to a less weird one. Unfortunately that means we need to
	 * use reflection as we want to be compatible with eclipse versions before
	 * 3.7 and 3.7+.
	 * 
	 * @param ref
	 *            The {@code foo} in {@code (String)foo}.
	 * @param castTo
	 *            The {@code String} in {@code (String)foo}.
	 */
	public static CastExpression makeCastExpression(final Expression ref, final TypeReference castTo, final ASTNode source) {
		CastExpression result;
		try {
			if (castExpressionConstructorIsTypeRefBased) {
				result = castExpressionConstructor.newInstance(ref, castTo);
			} else {
				Expression castToConverted = castTo;
				
				if (castTo.getClass() == SingleTypeReference.class && !isPrimitive(castTo)) {
					final SingleTypeReference str = (SingleTypeReference) castTo;
					// Why a SingleNameReference instead of a
					// SingleTypeReference you ask? I don't know. It seems dumb.
					// Ask the ecj guys.
					castToConverted = new SingleNameReference(str.token, 0);
					castToConverted.bits = (castToConverted.bits & ~Binding.VARIABLE) | Binding.TYPE;
					castToConverted.sourceStart = str.sourceStart;
					castToConverted.sourceEnd = str.sourceEnd;
					setGeneratedBy(castToConverted, source);
				} else if (castTo.getClass() == QualifiedTypeReference.class) {
					final QualifiedTypeReference qtr = (QualifiedTypeReference) castTo;
					// Same here, but for the more complex types, they stay
					// types.
					castToConverted = new QualifiedNameReference(qtr.tokens, copy(qtr.sourcePositions), qtr.sourceStart, qtr.sourceEnd);
					castToConverted.bits = (castToConverted.bits & ~Binding.VARIABLE) | Binding.TYPE;
					setGeneratedBy(castToConverted, source);
				}
				
				result = castExpressionConstructor.newInstance(ref, castToConverted);
			}
		} catch (final InvocationTargetException e) {
			throw Lombok.sneakyThrow(e.getCause());
		} catch (final IllegalAccessException e) {
			throw Lombok.sneakyThrow(e);
		} catch (final InstantiationException e) {
			throw Lombok.sneakyThrow(e);
		}
		
		result.sourceStart = source.sourceStart;
		result.sourceEnd = source.sourceEnd;
		result.statementEnd = source.sourceEnd;
		
		setGeneratedBy(result, source);
		return result;
	}
	
	private static final Constructor<CastExpression> castExpressionConstructor;
	private static final boolean castExpressionConstructorIsTypeRefBased;
	
	static {
		Constructor<?> constructor = null;
		for (final Constructor<?> ctor : CastExpression.class.getConstructors()) {
			if (ctor.getParameterTypes().length != 2) continue;
			constructor = ctor;
		}
		
		@SuppressWarnings("unchecked")
		final Constructor<CastExpression> castExpressionConstructor_ = (Constructor<CastExpression>) constructor;
		castExpressionConstructor = castExpressionConstructor_;
		
		castExpressionConstructorIsTypeRefBased = (castExpressionConstructor.getParameterTypes()[1] == TypeReference.class);
	}
	
	/**
	 * In eclipse 3.7+, IntLiterals are created using a factory-method
	 * Unfortunately that means we need to use reflection as we want to be
	 * compatible with eclipse versions before 3.7.
	 */
	public static IntLiteral makeIntLiteral(final char[] token, final ASTNode source) {
		final int pS = source == null ? 0 : source.sourceStart, pE = source == null ? 0 : source.sourceEnd;
		IntLiteral result;
		try {
			if (intLiteralConstructor != null) {
				result = intLiteralConstructor.newInstance(token, pS, pE);
			} else {
				result = (IntLiteral) intLiteralFactoryMethod.invoke(null, token, pS, pE);
			}
		} catch (final InvocationTargetException e) {
			throw Lombok.sneakyThrow(e.getCause());
		} catch (final IllegalAccessException e) {
			throw Lombok.sneakyThrow(e);
		} catch (final InstantiationException e) {
			throw Lombok.sneakyThrow(e);
		}
		
		if (source != null) setGeneratedBy(result, source);
		return result;
	}
	
	private static final Constructor<IntLiteral> intLiteralConstructor;
	private static final Method intLiteralFactoryMethod;
	
	static {
		final Class<?>[] parameterTypes = {char[].class, int.class, int.class};
		Constructor<IntLiteral> intLiteralConstructor_ = null;
		Method intLiteralFactoryMethod_ = null;
		try {
			intLiteralConstructor_ = IntLiteral.class.getConstructor(parameterTypes);
		} catch (final Throwable ignore) {
			// probably eclipse 3.7++
		}
		try {
			intLiteralFactoryMethod_ = IntLiteral.class.getMethod("buildIntLiteral", parameterTypes);
		} catch (final Throwable ignore) {
			// probably eclipse versions before 3.7
		}
		intLiteralConstructor = intLiteralConstructor_;
		intLiteralFactoryMethod = intLiteralFactoryMethod_;
	}
	
	private static boolean isAllValidOnXCharacters(final char[] in) {
		if (in == null || in.length == 0) return false;
		for (final char c : in)
			if (c != '_' && c != 'X' && c != 'x' && c != '$') return false;
		return true;
	}
	
	public static List<Annotation> unboxAndRemoveAnnotationParameter(final Annotation annotation, final String annotationName, final String errorName, final EclipseNode errorNode) {
		if ("value".equals(annotationName)) {
			// We can't unbox this, because SingleMemberAnnotation REQUIRES a
			// value, and this method
			// is supposed to remove the value. That means we need to replace
			// the SMA with either
			// MarkerAnnotation or NormalAnnotation and that is beyond the scope
			// of this method as we
			// don't need that at the time of writing this method; we only unbox
			// onMethod, onParameter
			// and onConstructor. Let's exit early and very obviously:
			throw new UnsupportedOperationException("Lombok cannot unbox 'value' from SingleMemberAnnotation at this time.");
		}
		if (!NormalAnnotation.class.equals(annotation.getClass())) {
			// Prevent MarkerAnnotation, SingleMemberAnnotation, and
			// CompletionOnAnnotationMemberValuePair from triggering this
			// handler.
			return Collections.emptyList();
		}
		
		final NormalAnnotation normalAnnotation = (NormalAnnotation) annotation;
		final MemberValuePair[] pairs = normalAnnotation.memberValuePairs;
		
		if (pairs == null) return Collections.emptyList();
		
		final char[] nameAsCharArray = annotationName.toCharArray();
		
		for (int i = 0; i < pairs.length; i++) {
			if (pairs[i].name == null || !Arrays.equals(nameAsCharArray, pairs[i].name)) continue;
			final Expression value = pairs[i].value;
			final MemberValuePair[] newPairs = new MemberValuePair[pairs.length - 1];
			if (i > 0) System.arraycopy(pairs, 0, newPairs, 0, i);
			if (i < pairs.length - 1) System.arraycopy(pairs, i + 1, newPairs, i, pairs.length - i - 1);
			normalAnnotation.memberValuePairs = newPairs;
			// We have now removed the annotation parameter and stored '@__({...
			// annotations ...})',
			// which we must now unbox.
			if (!(value instanceof Annotation)) {
				errorNode.addError("The correct format is " + errorName + "@__({@SomeAnnotation, @SomeOtherAnnotation}))");
				return Collections.emptyList();
			}
			
			final Annotation atDummyIdentifier = (Annotation) value;
			if (!(atDummyIdentifier.type instanceof SingleTypeReference) || !isAllValidOnXCharacters(((SingleTypeReference) atDummyIdentifier.type).token)) {
				errorNode.addError("The correct format is " + errorName + "@__({@SomeAnnotation, @SomeOtherAnnotation}))");
				return Collections.emptyList();
			}
			
			if (atDummyIdentifier instanceof MarkerAnnotation) {
				// It's @Getter(onMethod=@__). This is weird, but fine.
				return Collections.emptyList();
			}
			
			Expression content = null;
			
			if (atDummyIdentifier instanceof NormalAnnotation) {
				final MemberValuePair[] mvps = ((NormalAnnotation) atDummyIdentifier).memberValuePairs;
				if (mvps == null || mvps.length == 0) {
					// It's @Getter(onMethod=@__()). This is weird, but fine.
					return Collections.emptyList();
				}
				if (mvps.length == 1 && Arrays.equals("value".toCharArray(), mvps[0].name)) {
					content = mvps[0].value;
				}
			}
			
			if (atDummyIdentifier instanceof SingleMemberAnnotation) {
				content = ((SingleMemberAnnotation) atDummyIdentifier).memberValue;
			}
			
			if (content == null) {
				errorNode.addError("The correct format is " + errorName + "@__({@SomeAnnotation, @SomeOtherAnnotation}))");
				return Collections.emptyList();
			}
			
			if (content instanceof Annotation) {
				return Collections.singletonList((Annotation) content);
			} else if (content instanceof ArrayInitializer) {
				final Expression[] expressions = ((ArrayInitializer) content).expressions;
				final List<Annotation> result = new ArrayList<Annotation>();
				if (expressions != null) for (final Expression ex : expressions) {
					if (ex instanceof Annotation) result.add((Annotation) ex);
					else {
						errorNode.addError("The correct format is " + errorName + "@__({@SomeAnnotation, @SomeOtherAnnotation}))");
						return Collections.emptyList();
					}
				}
				return result;
			} else {
				errorNode.addError("The correct format is " + errorName + "@__({@SomeAnnotation, @SomeOtherAnnotation}))");
				return Collections.emptyList();
			}
		}
		
		return Collections.emptyList();
	}
	
	public static NameReference createNameReference(final String name, final Annotation source) {
		final int pS = source.sourceStart, pE = source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		
		final char[][] nameTokens = fromQualifiedName(name);
		final long[] pos = new long[nameTokens.length];
		Arrays.fill(pos, p);
		
		final QualifiedNameReference nameReference = new QualifiedNameReference(nameTokens, pos, pS, pE);
		nameReference.statementEnd = pE;
		
		setGeneratedBy(nameReference, source);
		return nameReference;
	}
	
	private static long[] copy(final long[] array) {
		return array == null ? null : array.clone();
	}
}
