/*
 * Copyright (C) 2009-2014 The Project Lombok Authors.
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
import static lombok.eclipse.handlers.EclipseHandlerUtil.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.AccessLevel;
import lombok.ConfigurationKeys;
import lombok.Getter;
import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.core.debug.FileLog;
import lombok.eclipse.EclipseAnnotationHandler;
import lombok.eclipse.EclipseNode;
import lombok.eclipse.agent.PatchDelegate;
import lombok.eclipse.handlers.EclipseHandlerUtil.FieldAccess;
import lombok.experimental.Delegate;

import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Assignment;
import org.eclipse.jdt.internal.compiler.ast.BinaryExpression;
import org.eclipse.jdt.internal.compiler.ast.Block;
import org.eclipse.jdt.internal.compiler.ast.CastExpression;
import org.eclipse.jdt.internal.compiler.ast.ConditionalExpression;
import org.eclipse.jdt.internal.compiler.ast.EqualExpression;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.IfStatement;
import org.eclipse.jdt.internal.compiler.ast.LocalDeclaration;
import org.eclipse.jdt.internal.compiler.ast.MessageSend;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.NullLiteral;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ReturnStatement;
import org.eclipse.jdt.internal.compiler.ast.SingleNameReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Statement;
import org.eclipse.jdt.internal.compiler.ast.SynchronizedStatement;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.mangosdk.spi.ProviderFor;

/**
 * Handles the {@code lombok.Getter} annotation for eclipse.
 */
@ProviderFor(EclipseAnnotationHandler.class) public class HandleGetter extends EclipseAnnotationHandler<Getter> {
	private static final Annotation[] EMPTY_ANNOTATIONS_ARRAY = new Annotation[0];
	
	public boolean generateGetterForType(final EclipseNode typeNode, final EclipseNode pos, final AccessLevel level, final boolean checkForTypeLevelGetter) {
		if (checkForTypeLevelGetter) {
			if (hasAnnotation(Getter.class, typeNode)) {
				// The annotation will make it happen, so we can skip it.
				return true;
			}
		}
		
		TypeDeclaration typeDecl = null;
		if (typeNode.get() instanceof TypeDeclaration) typeDecl = (TypeDeclaration) typeNode.get();
		final int modifiers = typeDecl == null ? 0 : typeDecl.modifiers;
		final boolean notAClass = (modifiers & (ClassFileConstants.AccInterface | ClassFileConstants.AccAnnotation)) != 0;
		
		if (typeDecl == null || notAClass) {
			pos.addError("@Getter is only supported on a class, an enum, or a field.");
			return false;
		}
		
		for (final EclipseNode field : typeNode.down()) {
			if (fieldQualifiesForGetterGeneration(field)) generateGetterForField(field, pos.get(), level, false);
		}
		return true;
	}
	
	public boolean fieldQualifiesForGetterGeneration(final EclipseNode field) {
		if (field.getKind() != Kind.FIELD) return false;
		final FieldDeclaration fieldDecl = (FieldDeclaration) field.get();
		return filterField(fieldDecl);
	}
	
	/**
	 * Generates a getter on the stated field.
	 * 
	 * Used by {@link HandleData}.
	 * 
	 * The difference between this call and the handle method is as follows:
	 * 
	 * If there is a {@code lombok.Getter} annotation on the field, it is used
	 * and the same rules apply (e.g. warning if the method already exists,
	 * stated access level applies). If not, the getter is still generated if it
	 * isn't already there, though there will not be a warning if its already
	 * there. The default access level is used.
	 */
	public void generateGetterForField(final EclipseNode fieldNode, final ASTNode pos, final AccessLevel level, final boolean lazy) {
		if (hasAnnotation(Getter.class, fieldNode)) {
			// The annotation will make it happen, so we can skip it.
			return;
		}
		
		createGetterForField(level, fieldNode, fieldNode, pos, false, lazy, Collections.<Annotation>emptyList());
	}
	
	@Override public void handle(final AnnotationValues<Getter> annotation, final Annotation ast, final EclipseNode annotationNode) {
		handleFlagUsage(annotationNode, ConfigurationKeys.GETTER_FLAG_USAGE, "@Getter");
		
		final EclipseNode node = annotationNode.up();
		final Getter annotationInstance = annotation.getInstance();
		final AccessLevel level = annotationInstance.value();
		final boolean lazy = annotationInstance.lazy();
		if (lazy) handleFlagUsage(annotationNode, ConfigurationKeys.GETTER_LAZY_FLAG_USAGE, "@Getter(lazy=true)");
		
		if (level == AccessLevel.NONE) {
			if (lazy) annotationNode.addWarning("'lazy' does not work with AccessLevel.NONE.");
			return;
		}
		
		if (node == null) return;
		
		final List<Annotation> onMethod = unboxAndRemoveAnnotationParameter(ast, "onMethod", "@Getter(onMethod=", annotationNode);
		
		switch (node.getKind()) {
		case FIELD:
			createGetterForFields(level, annotationNode.upFromAnnotationToFields(), annotationNode, annotationNode.get(), true, lazy, onMethod);
			break;
		case TYPE:
			if (!onMethod.isEmpty()) {
				annotationNode.addError("'onMethod' is not supported for @Getter on a type.");
			}
			if (lazy) annotationNode.addError("'lazy' is not supported for @Getter on a type.");
			generateGetterForType(node, annotationNode, level, false);
			break;
		}
	}
	
	public void createGetterForFields(final AccessLevel level, final Collection<EclipseNode> fieldNodes, final EclipseNode errorNode, final ASTNode source, final boolean whineIfExists, final boolean lazy, final List<Annotation> onMethod) {
		for (final EclipseNode fieldNode : fieldNodes) {
			createGetterForField(level, fieldNode, errorNode, source, whineIfExists, lazy, onMethod);
		}
	}
	
	public void createGetterForField(final AccessLevel level, final EclipseNode fieldNode, final EclipseNode errorNode, final ASTNode source, final boolean whineIfExists, final boolean lazy, final List<Annotation> onMethod) {
		if (fieldNode.getKind() != Kind.FIELD) {
			errorNode.addError("@Getter is only supported on a class or a field.");
			return;
		}
		
		final FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		if (lazy) {
			if ((field.modifiers & ClassFileConstants.AccPrivate) == 0 || (field.modifiers & ClassFileConstants.AccFinal) == 0) {
				errorNode.addError("'lazy' requires the field to be private and final.");
				return;
			}
			if (field.initialization == null) {
				errorNode.addError("'lazy' requires field initialization.");
				return;
			}
		}
		
		final TypeReference fieldType = copyType(field.type, source);
		final boolean isBoolean = isBoolean(fieldType);
		final String getterName = toGetterName(fieldNode, isBoolean);
		
		if (fieldNode.getAst().getFileName().equals("domain/models/fileshare/vo/createuser/CreateUser.java")) {
			FileLog.log(String.format("getterName=%s @ createGetterForField(..) //file %s", getterName, fieldNode.getAst().getFileName()));
		}
		
		if (getterName == null) {
			errorNode.addWarning("Not generating getter for this field: It does not fit your @Accessors prefix list.");
			return;
		}
		
		final int modifier = toEclipseModifier(level) | (field.modifiers & ClassFileConstants.AccStatic);
		
		for (final String altName : toAllGetterNames(fieldNode, isBoolean)) {
			switch (methodExists(altName, fieldNode, false, 0)) {
			case EXISTS_BY_LOMBOK:
				return;
			case EXISTS_BY_USER:
				if (whineIfExists) {
					String altNameExpl = "";
					if (!altName.equals(getterName)) altNameExpl = String.format(" (%s)", altName);
					errorNode.addWarning(String.format("Not generating %s(): A method with that name already exists%s", getterName, altNameExpl));
				}
				return;
			default:
			case NOT_EXISTS:
				// continue scanning the other alt names.
			}
		}
		
		final MethodDeclaration method = createGetter((TypeDeclaration) fieldNode.up().get(), fieldNode, getterName, modifier, source, lazy, onMethod);
		
		injectMethod(fieldNode.up(), method);
	}
	
	public static Annotation[] findDelegatesAndMarkAsHandled(final EclipseNode fieldNode) {
		final List<Annotation> delegates = new ArrayList<Annotation>();
		for (final EclipseNode child : fieldNode.down()) {
			if (annotationTypeMatches(Delegate.class, child)) {
				final Annotation delegate = (Annotation) child.get();
				PatchDelegate.markHandled(delegate);
				delegates.add(delegate);
			}
		}
		return delegates.toArray(EMPTY_ANNOTATIONS_ARRAY);
	}
	
	public MethodDeclaration createGetter(final TypeDeclaration parent, final EclipseNode fieldNode, final String name, final int modifier, final ASTNode source, final boolean lazy, final List<Annotation> onMethod) {
		final FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		
		// Remember the type; lazy will change it;
		final TypeReference returnType = copyType(((FieldDeclaration) fieldNode.get()).type, source);
		
		Statement[] statements;
		if (lazy) {
			statements = createLazyGetterBody(source, fieldNode);
		} else {
			statements = createSimpleGetterBody(source, fieldNode);
		}
		
		final MethodDeclaration method = new MethodDeclaration(parent.compilationResult);
		method.modifiers = modifier;
		method.returnType = returnType;
		method.annotations = null;
		method.arguments = null;
		method.selector = name.toCharArray();
		method.binding = null;
		method.thrownExceptions = null;
		method.typeParameters = null;
		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
		method.bodyStart = method.declarationSourceStart = method.sourceStart = source.sourceStart;
		method.bodyEnd = method.declarationSourceEnd = method.sourceEnd = source.sourceEnd;
		method.statements = statements;
		
		EclipseHandlerUtil.registerCreatedLazyGetter((FieldDeclaration) fieldNode.get(), method.selector, returnType);
		
		/*
		 * Generate annotations that must be put on the generated method, and
		 * attach them.
		 */{
			Annotation[] deprecated = null;
			if (isFieldDeprecated(fieldNode)) {
				deprecated = new Annotation[] {generateDeprecatedAnnotation(source)};
			}
			
			method.annotations = copyAnnotations(source, onMethod.toArray(new Annotation[0]), findAnnotations(field, NON_NULL_PATTERN), findAnnotations(field, NULLABLE_PATTERN), findDelegatesAndMarkAsHandled(fieldNode), deprecated);
		}
		
		method.traverse(new SetGeneratedByVisitor(source), parent.scope);
		return method;
	}
	
	public Statement[] createSimpleGetterBody(final ASTNode source, final EclipseNode fieldNode) {
		final FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		final Expression fieldRef = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
		final Statement returnStatement = new ReturnStatement(fieldRef, field.sourceStart, field.sourceEnd);
		return new Statement[] {returnStatement};
	}
	
	private static final char[][] AR = fromQualifiedName("java.util.concurrent.atomic.AtomicReference");
	
	public static final java.util.Map<String, char[][]> TYPE_MAP;
	static {
		final Map<String, char[][]> m = new HashMap<String, char[][]>();
		m.put("int", fromQualifiedName("java.lang.Integer"));
		m.put("double", fromQualifiedName("java.lang.Double"));
		m.put("float", fromQualifiedName("java.lang.Float"));
		m.put("short", fromQualifiedName("java.lang.Short"));
		m.put("byte", fromQualifiedName("java.lang.Byte"));
		m.put("long", fromQualifiedName("java.lang.Long"));
		m.put("boolean", fromQualifiedName("java.lang.Boolean"));
		m.put("char", fromQualifiedName("java.lang.Character"));
		TYPE_MAP = Collections.unmodifiableMap(m);
	}
	
	private static char[] valueName = "value".toCharArray();
	private static char[] actualValueName = "actualValue".toCharArray();
	
	private static final int PARENTHESIZED = (1 << ASTNode.ParenthesizedSHIFT) & ASTNode.ParenthesizedMASK;
	
	public Statement[] createLazyGetterBody(final ASTNode source, final EclipseNode fieldNode) {
		/*
		 * java.lang.Object value = this.fieldName.get(); if (value == null) {
		 * synchronized (this.fieldName) { value = this.fieldName.get(); if
		 * (value == null) { final RawValueType actualValue =
		 * INITIALIZER_EXPRESSION; [IF PRIMITIVE] value = actualValue; [ELSE]
		 * value = actualValue == null ? this.fieldName : actualValue; [END IF]
		 * this.fieldName.set(value); } } } [IF PRIMITIVE] return
		 * (BoxedValueType) value; [ELSE] return (BoxedValueType) (value ==
		 * this.fieldName ? null : value); [END IF]
		 */
		
		final FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		final int pS = source.sourceStart, pE = source.sourceEnd;
		final long p = (long) pS << 32 | pE;
		
		final TypeReference rawComponentType = copyType(field.type, source);
		TypeReference boxedComponentType = null;
		boolean isPrimitive = false;
		if (field.type instanceof SingleTypeReference && !(field.type instanceof ArrayTypeReference)) {
			final char[][] newType = TYPE_MAP.get(new String(((SingleTypeReference) field.type).token));
			if (newType != null) {
				boxedComponentType = new QualifiedTypeReference(newType, poss(source, 3));
				isPrimitive = true;
			}
		}
		if (boxedComponentType == null) boxedComponentType = copyType(field.type, source);
		boxedComponentType.sourceStart = pS;
		boxedComponentType.sourceEnd = boxedComponentType.statementEnd = pE;
		
		final Statement[] statements = new Statement[3];
		
		/* java.lang.Object value = this.fieldName.get(); */{
			final LocalDeclaration valueDecl = new LocalDeclaration(valueName, pS, pE);
			valueDecl.type = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(source, 3));
			valueDecl.type.sourceStart = pS;
			valueDecl.type.sourceEnd = valueDecl.type.statementEnd = pE;
			
			final MessageSend getter = new MessageSend();
			getter.sourceStart = pS;
			getter.statementEnd = getter.sourceEnd = pE;
			getter.selector = new char[] {'g', 'e', 't'};
			getter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
			
			valueDecl.initialization = getter;
			statements[0] = valueDecl;
		}
		
		/*
		 * if (value == null) { synchronized (this.fieldName) { value =
		 * this.fieldName.get(); if (value == null) { final ValueType
		 * actualValue = INITIALIZER_EXPRESSION; [IF PRIMITIVE] value =
		 * actualValue; [ELSE] value = actualValue == null ? this.fieldName :
		 * actualValue; [END IF] this.fieldName.set(value); } } }
		 */{
			final EqualExpression cond = new EqualExpression(new SingleNameReference(valueName, p), new NullLiteral(pS, pE), BinaryExpression.EQUAL_EQUAL);
			final Block then = new Block(0);
			final Expression lock = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
			final Block inner = new Block(0);
			inner.statements = new Statement[2];
			/* value = this.fieldName.get(); */{
				final MessageSend getter = new MessageSend();
				getter.sourceStart = pS;
				getter.sourceEnd = getter.statementEnd = pE;
				getter.selector = new char[] {'g', 'e', 't'};
				getter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
				final Assignment assign = new Assignment(new SingleNameReference(valueName, p), getter, pE);
				assign.sourceStart = pS;
				assign.statementEnd = assign.sourceEnd = pE;
				inner.statements[0] = assign;
			}
			/* if (value == null) */{
				final EqualExpression innerCond = new EqualExpression(new SingleNameReference(valueName, p), new NullLiteral(pS, pE), BinaryExpression.EQUAL_EQUAL);
				innerCond.sourceStart = pS;
				innerCond.sourceEnd = innerCond.statementEnd = pE;
				final Block innerThen = new Block(0);
				innerThen.statements = new Statement[3];
				/* final ValueType actualValue = INITIALIZER_EXPRESSION */{
					final LocalDeclaration actualValueDecl = new LocalDeclaration(actualValueName, pS, pE);
					actualValueDecl.type = rawComponentType;
					actualValueDecl.type.sourceStart = pS;
					actualValueDecl.type.sourceEnd = actualValueDecl.type.statementEnd = pE;
					actualValueDecl.initialization = field.initialization;
					actualValueDecl.modifiers = ClassFileConstants.AccFinal;
					innerThen.statements[0] = actualValueDecl;
				}
				/* [IF PRIMITIVE] value = actualValue; */{
					if (isPrimitive) {
						final Assignment innerAssign = new Assignment(new SingleNameReference(valueName, p), new SingleNameReference(actualValueName, p), pE);
						innerAssign.sourceStart = pS;
						innerAssign.statementEnd = innerAssign.sourceEnd = pE;
						innerThen.statements[1] = innerAssign;
					}
				}
				/*
				 * [ELSE] value = actualValue == null ? this.fieldName :
				 * actualValue;
				 */{
					if (!isPrimitive) {
						final EqualExpression avIsNull = new EqualExpression(new SingleNameReference(actualValueName, p), new NullLiteral(pS, pE), BinaryExpression.EQUAL_EQUAL);
						avIsNull.sourceStart = pS;
						avIsNull.sourceEnd = avIsNull.statementEnd = pE;
						final Expression fieldRef = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
						final ConditionalExpression ternary = new ConditionalExpression(avIsNull, fieldRef, new SingleNameReference(actualValueName, p));
						ternary.sourceStart = pS;
						ternary.sourceEnd = ternary.statementEnd = pE;
						final Assignment innerAssign = new Assignment(new SingleNameReference(valueName, p), ternary, pE);
						innerAssign.sourceStart = pS;
						innerAssign.statementEnd = innerAssign.sourceEnd = pE;
						innerThen.statements[1] = innerAssign;
					}
				}
				
				/* this.fieldName.set(value); */{
					final MessageSend setter = new MessageSend();
					setter.sourceStart = pS;
					setter.sourceEnd = setter.statementEnd = pE;
					setter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
					setter.selector = new char[] {'s', 'e', 't'};
					setter.arguments = new Expression[] {new SingleNameReference(valueName, p)};
					innerThen.statements[2] = setter;
				}
				
				final IfStatement innerIf = new IfStatement(innerCond, innerThen, pS, pE);
				inner.statements[1] = innerIf;
			}
			
			final SynchronizedStatement sync = new SynchronizedStatement(lock, inner, pS, pE);
			then.statements = new Statement[] {sync};
			
			final IfStatement ifStatement = new IfStatement(cond, then, pS, pE);
			statements[1] = ifStatement;
		}
		
		/* [IF PRIMITIVE] return (BoxedValueType)value; */{
			if (isPrimitive) {
				final CastExpression cast = makeCastExpression(new SingleNameReference(valueName, p), boxedComponentType, source);
				statements[2] = new ReturnStatement(cast, pS, pE);
			}
		}
		/*
		 * [ELSE] return (BoxedValueType)(value == this.fieldName ? null :
		 * value);
		 */{
			if (!isPrimitive) {
				final EqualExpression vIsThisFieldName = new EqualExpression(new SingleNameReference(valueName, p), createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source), BinaryExpression.EQUAL_EQUAL);
				vIsThisFieldName.sourceStart = pS;
				vIsThisFieldName.sourceEnd = vIsThisFieldName.statementEnd = pE;
				final ConditionalExpression ternary = new ConditionalExpression(vIsThisFieldName, new NullLiteral(pS, pE), new SingleNameReference(valueName, p));
				ternary.sourceStart = pS;
				ternary.sourceEnd = ternary.statementEnd = pE;
				ternary.bits |= PARENTHESIZED;
				final CastExpression cast = makeCastExpression(ternary, boxedComponentType, source);
				statements[2] = new ReturnStatement(cast, pS, pE);
			}
		}
		
		// update the field type and init last
		
		/*
		 * private final
		 * java.util.concurrent.atomic.AtomicReference<java.lang.Object>
		 * fieldName = new
		 * java.util.concurrent.atomic.AtomicReference<java.lang.Object>();
		 */{
			final TypeReference innerType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(source, 3));
			final TypeReference[][] typeParams = new TypeReference[5][];
			typeParams[4] = new TypeReference[] {innerType};
			final TypeReference type = new ParameterizedQualifiedTypeReference(AR, typeParams, 0, poss(source, 5));
			
			// Some magic here
			type.sourceStart = -1;
			type.sourceEnd = -2;
			
			field.type = type;
			final AllocationExpression init = new AllocationExpression();
			// Some magic here
			init.sourceStart = field.initialization.sourceStart;
			init.sourceEnd = init.statementEnd = field.initialization.sourceEnd;
			init.type = copyType(type, source);
			field.initialization = init;
		}
		return statements;
	}
}
