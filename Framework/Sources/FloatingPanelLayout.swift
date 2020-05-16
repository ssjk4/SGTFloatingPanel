//
//  Created by Shin Yamamoto on 2018/09/27.
//  Copyright © 2018 Shin Yamamoto. All rights reserved.
//

import UIKit

@objc public enum FloatingPanelLayoutReferenceGuide: Int {
    case superview = 0
    case safeArea = 1
}

extension FloatingPanelLayoutReferenceGuide {
    func layoutGuide(vc: UIViewController) -> LayoutGuideProvider {
        switch self {
        case .safeArea:
            return vc.fp_safeAreaLayoutGuide
        case .superview:
            return vc.view
        }
    }
}

@objc public enum FloatingPanelPosition: Int {
    case top
    //case left
    case bottom
    //case right
}

@objc public enum FloatingPanelReferenceEdge: Int {
    case top
    //case left
    case bottom
    //case right
}

@objc public protocol FloatingPanelLayoutAnchoring {
    var referenceGuide: FloatingPanelLayoutReferenceGuide { get }
    func layoutConstraints(_ fpc: FloatingPanelController, for position: FloatingPanelPosition) -> [NSLayoutConstraint]
}

@objc final public class FloatingPanelLayoutAnchor: NSObject, FloatingPanelLayoutAnchoring /*, NSCopying */ {
    @objc public init(absoluteInset: CGFloat, edge: FloatingPanelReferenceEdge, referenceGuide: FloatingPanelLayoutReferenceGuide) {
        self.inset = absoluteInset
        self.referenceGuide = referenceGuide
        self.referenceEdge = edge
        self.isAbsolute = true
    }

    @objc public init(fractionalInset: CGFloat, edge: FloatingPanelReferenceEdge, referenceGuide: FloatingPanelLayoutReferenceGuide) {
        self.inset = fractionalInset
        self.referenceGuide = referenceGuide
        self.referenceEdge = edge
        self.isAbsolute = false
    }
    fileprivate let inset: CGFloat
    fileprivate let isAbsolute: Bool
    @objc public let referenceGuide: FloatingPanelLayoutReferenceGuide
    @objc public let referenceEdge: FloatingPanelReferenceEdge
}

public extension FloatingPanelLayoutAnchor {
    func layoutConstraints(_ vc: FloatingPanelController, for position: FloatingPanelPosition) -> [NSLayoutConstraint] {
        let layoutGuide = referenceGuide.layoutGuide(vc: vc)
        switch position {
        case .top:
            return layoutConstraints(layoutGuide, for: vc.surfaceView.bottomAnchor)
        case .bottom:
            return layoutConstraints(layoutGuide, for:  vc.surfaceView.topAnchor)
        }
    }

    private func layoutConstraints(_ layoutGuide: LayoutGuideProvider, for edgeAnchor: NSLayoutYAxisAnchor) -> [NSLayoutConstraint] {
        switch referenceEdge {
        case .top:
            if isAbsolute {
                return [edgeAnchor.constraint(equalTo: layoutGuide.topAnchor, constant: inset)]
            }
            let offsetAnchor = layoutGuide.topAnchor.anchorWithOffset(to: edgeAnchor)
            return [offsetAnchor.constraint(equalTo:layoutGuide.heightAnchor, multiplier: inset)]
        case .bottom:
            if isAbsolute {
                return [layoutGuide.bottomAnchor.constraint(equalTo: edgeAnchor, constant: inset)]
            }
            let offsetAnchor = edgeAnchor.anchorWithOffset(to: layoutGuide.bottomAnchor)
            return [offsetAnchor.constraint(equalTo: layoutGuide.heightAnchor, multiplier: inset)]
        default:
            fatalError("Unsupported reference edges")
        }
    }
}

@objc final public class FloatingPanelIntrinsicLayoutAnchor: NSObject, FloatingPanelLayoutAnchoring /*, NSCopying */ {
    @objc public init(absoluteOffset offset: CGFloat, referenceGuide: FloatingPanelLayoutReferenceGuide = .safeArea) {
        self.offset = offset
        self.referenceGuide = referenceGuide
        self.isAbsolute = true
    }
    // offset = 0.0 -> All content visible
    // offset = 1.0 -> All content invisible
    @objc public init(fractionalOffset offset: CGFloat, referenceGuide: FloatingPanelLayoutReferenceGuide = .safeArea) {
        self.offset = offset
        self.referenceGuide = referenceGuide
        self.isAbsolute = false
    }
    fileprivate let offset: CGFloat
    fileprivate let isAbsolute: Bool
    @objc public let referenceGuide: FloatingPanelLayoutReferenceGuide
}

public extension FloatingPanelIntrinsicLayoutAnchor {
    func layoutConstraints(_ vc: FloatingPanelController, for position: FloatingPanelPosition) -> [NSLayoutConstraint] {
        let surfaceIntrinsicLength = position.mainDimension(vc.surfaceView.intrinsicContentSize)
        let constant = isAbsolute ? surfaceIntrinsicLength - offset : surfaceIntrinsicLength * (1 - offset)
        let layoutGuide = referenceGuide.layoutGuide(vc: vc)
        switch position {
        case .top:
            return [vc.surfaceView.bottomAnchor.constraint(equalTo: layoutGuide.topAnchor, constant: constant)]
        case .bottom:
            return [vc.surfaceView.topAnchor.constraint(equalTo: layoutGuide.bottomAnchor, constant: -constant)]
        }
    }
}

@objc public protocol FloatingPanelLayout {
    /// TODO: Write doc comment
    @objc var anchorPosition: FloatingPanelPosition { get }

    /// TODO: Write doc comment
    @objc var initialState: FloatingPanelState { get }

    /// TODO: Write doc comment
    @objc var stateAnchors: [FloatingPanelState: FloatingPanelLayoutAnchoring] { get }

    /// Returns X-axis and width layout constraints of the surface view of a floating panel.
    /// You must not include any Y-axis and height layout constraints of the surface view
    /// because their constraints will be configured by the floating panel controller.
    /// By default, the width of a surface view fits a safe area.
    @objc optional func prepareLayout(surfaceView: UIView, in view: UIView) -> [NSLayoutConstraint]

    /// Returns a CGFloat value to determine the backdrop view's alpha for a state.
    ///
    /// Default is 0.3 at full state, otherwise 0.0.
    @objc optional func backdropAlpha(for state: FloatingPanelState) -> CGFloat
}

@objcMembers
open class FloatingPanelBottomLayout: NSObject, FloatingPanelLayout {
    public override init() {
        super.init()
    }
    open var initialState: FloatingPanelState {
        return .half
    }

    open var stateAnchors: [FloatingPanelState: FloatingPanelLayoutAnchoring]  {
        return [
            .full: FloatingPanelLayoutAnchor(absoluteInset: 18.0, edge: .top, referenceGuide: .safeArea),
            .half: FloatingPanelLayoutAnchor(fractionalInset: 0.5, edge: .bottom, referenceGuide: .safeArea),
            .tip: FloatingPanelLayoutAnchor(absoluteInset: 69.0, edge: .bottom, referenceGuide: .safeArea),
        ]
    }

    open var anchorPosition: FloatingPanelPosition {
        return .bottom
    }

    open func prepareLayout(surfaceView: UIView, in view: UIView) -> [NSLayoutConstraint] {
        return [
            surfaceView.leftAnchor.constraint(equalTo: view.sideLayoutGuide.leftAnchor, constant: 0.0),
            surfaceView.rightAnchor.constraint(equalTo: view.sideLayoutGuide.rightAnchor, constant: 0.0),
        ]
    }

    open func backdropAlpha(for state: FloatingPanelState) -> CGFloat {
        return state == .full ? 0.3 : 0.0
    }
}

struct LayoutSegment {
    let lower: FloatingPanelState?
    let upper: FloatingPanelState?
}

class FloatingPanelLayoutAdapter {
    weak var vc: FloatingPanelController!
    private weak var surfaceView: FloatingPanelSurfaceView!
    private weak var backdropView: FloatingPanelBackdropView!
    private let defaultLayout = FloatingPanelBottomLayout()

    var layout: FloatingPanelLayout {
        didSet {
            surfaceView.anchorPosition = layout.anchorPosition
        }
    }

    private var safeAreaInsets: UIEdgeInsets {
        return vc?.fp_safeAreaInsets ?? .zero
    }

    private var initialConst: CGFloat = 0.0

    private var fixedConstraints: [NSLayoutConstraint] = []
    private var fullConstraints: [NSLayoutConstraint] = []
    private var halfConstraints: [NSLayoutConstraint] = []
    private var tipConstraints: [NSLayoutConstraint] = []
    private var offConstraints: [NSLayoutConstraint] = []
    private var fitToBoundsConstraint: NSLayoutConstraint?

    private(set) var interactionEdgeConstraint: NSLayoutConstraint?
    private(set) var animationEdgeConstraint: NSLayoutConstraint?

    private var heightConstraint: NSLayoutConstraint?

    private var activeStates: Set<FloatingPanelState> {
        return Set(layout.stateAnchors.keys)
    }

    var orderedStates: [FloatingPanelState] {
        return activeStates.sorted(by: {
            return $0.order < $1.order
        })
    }

    var validStates: Set<FloatingPanelState> {
        return activeStates.union([.hidden])
    }

    var sortedDirectionalStates: [FloatingPanelState] {
        return activeStates.sorted(by: {
            switch layout.anchorPosition {
            case .top:
                return $0.order < $1.order
            case .bottom:
                return $0.order > $1.order
            }
        })
    }

    private var directionalLeastState: FloatingPanelState {
        return sortedDirectionalStates.first ?? .hidden
    }

    private var directionalMostState: FloatingPanelState {
        return sortedDirectionalStates.last ?? .hidden
    }

    var edgeLeastState: FloatingPanelState {
        return orderedStates.first ?? .hidden
    }
    
    var edgeMostState: FloatingPanelState {
        return orderedStates.last ?? .hidden
    }

    var edgeMostY: CGFloat {
        return positionY(for: edgeMostState)
    }

    var adjustedContentInsets: UIEdgeInsets {
        switch layout.anchorPosition {
        case .top:
            return UIEdgeInsets(top: safeAreaInsets.top,
                                left: 0.0,
                                bottom: 0.0,
                                right: 0.0)
        case .bottom:
            return UIEdgeInsets(top: 0.0,
                                left: 0.0,
                                bottom: safeAreaInsets.bottom,
                                right: 0.0)
        }
    }

    /*
    Returns a constraint based value in the interaction and animation.

    So that it doesn't need to call `surfaceView.layoutIfNeeded()`
    after every interaction and animation update. It has an effect on
    the smooth interaction because the content view doesn't need to update
    its layout frequently.
    */
    var surfaceEdgeLocation: CGPoint {
        get {
            if let interactionConstraint = interactionEdgeConstraint {
                return CGPoint(x: 0.0, y: interactionConstraint.constant)
            }
            if let animationConstraint = animationEdgeConstraint, let anchor = layout.stateAnchors[vc.state] {
                switch referenceEdge(of: anchor) {
                case .top:
                    if anchor.referenceGuide == .safeArea {
                        return CGPoint(x: 0.0, y: animationConstraint.constant + safeAreaInsets.top)
                    } else {
                        return CGPoint(x: 0.0, y: animationConstraint.constant)
                    }
                case .bottom:
                    if anchor.referenceGuide == .safeArea {
                        return CGPoint(x: 0.0, y: vc.view.bounds.height + animationConstraint.constant - safeAreaInsets.bottom)
                    } else {
                        return CGPoint(x: 0.0, y: vc.view.bounds.height + animationConstraint.constant)
                    }
                default:
                    break
                }
            }
            let displayScale = surfaceView.traitCollection.displayScale
            return CGPoint(x: 0.0, y: displayTrunc(edgeY(surfaceView.frame), by: displayScale))
        }
        set {
            if let interactionConstraint = interactionEdgeConstraint {
                interactionConstraint.constant = newValue.y
            } else if let animationConstraint = animationEdgeConstraint, let anchor = layout.stateAnchors[vc.state] {
                switch referenceEdge(of: anchor) {
                case .top:
                    animationConstraint.constant = newValue.y
                    if anchor.referenceGuide == .safeArea {
                        animationConstraint.constant -= safeAreaInsets.top
                    }
                case .bottom:
                    animationConstraint.constant = newValue.y - vc.view.bounds.height
                    if anchor.referenceGuide == .safeArea {
                           animationConstraint.constant += safeAreaInsets.bottom
                    }
                default:
                    break
                }
            } else {
                switch layout.anchorPosition {
                case .top:
                    return surfaceView.frame.origin.y = newValue.y - surfaceView.bounds.height
                case .bottom:
                    return surfaceView.frame.origin.y = newValue.y
                }
            }
        }
    }

    var surfaceMostLocation: CGPoint {
        return surfaceEdgeLocation(for: edgeMostState)
    }

    var surfaceLeastLocation: CGPoint {
        return surfaceEdgeLocation(for: edgeLeastState)
    }

    var offsetFromEdgeMost: CGFloat {
        switch layout.anchorPosition {
        case .top:
            return surfaceView.presentationFrame.maxY - positionY(for: directionalMostState)
        case .bottom:
            return positionY(for: directionalLeastState) - surfaceView.presentationFrame.minY
        }
    }

    var hiddenAnchor: FloatingPanelLayoutAnchoring {
        switch layout.anchorPosition {
        case .top:
            return FloatingPanelLayoutAnchor(absoluteInset: 0, edge: .top, referenceGuide: .superview)
        case .bottom:
            return FloatingPanelLayoutAnchor(absoluteInset: 0, edge: .bottom, referenceGuide: .superview)
        }
    }

    init(vc: FloatingPanelController,
         surfaceView: FloatingPanelSurfaceView,
         backdropView: FloatingPanelBackdropView,
         layout: FloatingPanelLayout) {
        self.vc = vc
        self.layout = layout
        self.surfaceView = surfaceView
        self.backdropView = backdropView
    }

    func surfaceEdgeLocation(for state: FloatingPanelState) -> CGPoint {
        return CGPoint(x: 0.0,
                       y: displayTrunc(positionY(for: state), by: surfaceView.traitCollection.displayScale))
    }

    func positionY(for state: FloatingPanelState) -> CGFloat {
        let bounds = vc.view.bounds
        let anchor = layout.stateAnchors[state] ?? self.hiddenAnchor

        var referenceBounds = bounds
        if anchor.referenceGuide == .safeArea {
            referenceBounds = referenceBounds.inset(by: safeAreaInsets)
        }
        switch anchor {
        case let ianchor as FloatingPanelIntrinsicLayoutAnchor:
            let referenceBoundsLength = mainDimension(referenceBounds.size)
            let surfaceIntrinsicLength = mainDimension(surfaceView.intrinsicContentSize)
            let diff = ianchor.isAbsolute ? ianchor.offset : surfaceIntrinsicLength * ianchor.offset
            switch layout.anchorPosition {
            case .top:
                return referenceBoundsLength - surfaceIntrinsicLength - diff
            case .bottom:
                return referenceBoundsLength - surfaceIntrinsicLength + diff
            }
        case let anchor as FloatingPanelLayoutAnchor:
            let diff = anchor.isAbsolute ? anchor.inset : mainDimension(referenceBounds.size) * anchor.inset
            switch anchor.referenceEdge {
            case .top:
                return referenceBounds.minY + diff
            case .bottom:
                return referenceBounds.maxY - diff
            }
        default:
            fatalError("Unsupported a FloatingPanelLayoutAnchoring object")
        }
     }

    func edgeY(_ frame: CGRect) -> CGFloat {
        switch layout.anchorPosition {
        case .top:
            return frame.maxY
        case .bottom:
            return frame.minY
        }
    }

    func mainDimension(_ size: CGSize) -> CGFloat {
        switch layout.anchorPosition {
        case .top, .bottom:
            return size.height
        }
    }

    func referenceEdge(of anchor: FloatingPanelLayoutAnchoring) -> FloatingPanelReferenceEdge {
        switch anchor {
        case is FloatingPanelIntrinsicLayoutAnchor:
            switch layout.anchorPosition {
            case .top: return .top
            case .bottom: return .bottom
            }
        case let anchor as FloatingPanelLayoutAnchor:
            return anchor.referenceEdge
        default:
            fatalError("Unsupported a FloatingPanelLayoutAnchoring object")
        }
    }

    func prepareLayout() {
        NSLayoutConstraint.deactivate(fixedConstraints)

        surfaceView.translatesAutoresizingMaskIntoConstraints = false
        backdropView.translatesAutoresizingMaskIntoConstraints = false

        // Fixed constraints of surface and backdrop views
        let surfaceConstraints = layout.prepareLayout?(surfaceView: surfaceView, in: vc.view!) ?? defaultLayout.prepareLayout(surfaceView: surfaceView, in: vc.view!)
        let backdropConstraints = [
            backdropView.topAnchor.constraint(equalTo: vc.view.topAnchor, constant: 0.0),
            backdropView.leftAnchor.constraint(equalTo: vc.view.leftAnchor,constant: 0.0),
            backdropView.rightAnchor.constraint(equalTo: vc.view.rightAnchor, constant: 0.0),
            backdropView.bottomAnchor.constraint(equalTo: vc.view.bottomAnchor, constant: 0.0),
            ]

        fixedConstraints = surfaceConstraints + backdropConstraints

        NSLayoutConstraint.deactivate(constraint: self.fitToBoundsConstraint)
        self.fitToBoundsConstraint = nil

        if vc.contentMode == .fitToBounds {
            fitToBoundsConstraint = {
                switch layout.anchorPosition {
                case .top:
                    return surfaceView.topAnchor.constraint(equalTo: vc.view.topAnchor, constant: 0.0)
                case .bottom:
                    return surfaceView.bottomAnchor.constraint(equalTo: vc.view.bottomAnchor, constant: 0.0)
                }
            }()
        }

        NSLayoutConstraint.deactivate(fullConstraints + halfConstraints + tipConstraints + offConstraints)

        if let fullAnchor = layout.stateAnchors[.full] {
            fullConstraints = fullAnchor.layoutConstraints(vc, for: layout.anchorPosition)
        }
        if let halfAnchor = layout.stateAnchors[.half] {
            halfConstraints = halfAnchor.layoutConstraints(vc, for: layout.anchorPosition)
        }
        if let tipAnchors = layout.stateAnchors[.tip] {
            tipConstraints = tipAnchors.layoutConstraints(vc, for: layout.anchorPosition)
        }
        let hiddenAnchor = layout.stateAnchors[.hidden] ?? self.hiddenAnchor
        offConstraints = hiddenAnchor.layoutConstraints(vc, for: layout.anchorPosition)
    }

    func startInteraction(at state: FloatingPanelState, offset: CGPoint = .zero) {
        if let edgeConstraint = self.interactionEdgeConstraint {
            initialConst = edgeConstraint.constant
            return
        }

        tearDownAnimationEdgeConstraint()

        NSLayoutConstraint.deactivate(fullConstraints + halfConstraints + tipConstraints + offConstraints)

        initialConst = edgeY(surfaceView.frame) + offset.y

        let edgeAnchor: NSLayoutYAxisAnchor
        switch layout.anchorPosition {
        case .top:
            edgeAnchor = surfaceView.bottomAnchor
        case .bottom:
            edgeAnchor = surfaceView.topAnchor
        }

        let edgeConst = edgeAnchor.constraint(equalTo: vc.view.topAnchor, constant: initialConst)

        NSLayoutConstraint.activate([edgeConst])
        self.interactionEdgeConstraint = edgeConst
    }

    func endInteraction(at state: FloatingPanelState) {
        // Don't deactivate `interactiveTopConstraint` here because it leads to
        // unsatisfiable constraints

        if self.interactionEdgeConstraint == nil {
            // Actiavate `interactiveTopConstraint` for `fitToBounds` mode.
            // It goes throught this path when the pan gesture state jumps
            // from .begin to .end.
            startInteraction(at: state)
        }
    }

    func setUpAnimationEdgeConstraint(to state: FloatingPanelState) -> (NSLayoutConstraint, CGFloat) {
        NSLayoutConstraint.deactivate(constraint: animationEdgeConstraint)

        let anchor = layout.stateAnchors[state] ?? self.hiddenAnchor

        NSLayoutConstraint.deactivate(fullConstraints + halfConstraints + tipConstraints + offConstraints)
        NSLayoutConstraint.deactivate(constraint: interactionEdgeConstraint)
        interactionEdgeConstraint = nil

        let layoutGuideProvider: LayoutGuideProvider
        switch anchor.referenceGuide {
        case .safeArea:
            layoutGuideProvider = vc.fp_safeAreaLayoutGuide
        case .superview:
            layoutGuideProvider = vc.view
        }
        let currentY = surfaceEdgeLocation.y
        let animationConstraint: NSLayoutConstraint
        var targetY = positionY(for: state)

        switch layout.anchorPosition {
        case .top:
            switch referenceEdge(of: anchor) {
            case .top:
                animationConstraint = surfaceView.bottomAnchor.constraint(equalTo: layoutGuideProvider.topAnchor,
                                                                          constant: currentY)
                if anchor.referenceGuide == .safeArea {
                    animationConstraint.constant -= safeAreaInsets.top
                    targetY -= safeAreaInsets.top
                }
            case .bottom:
                let baseHeight = vc.view.bounds.height
                targetY = -(baseHeight - targetY)
                animationConstraint = surfaceView.bottomAnchor.constraint(equalTo: layoutGuideProvider.bottomAnchor,
                                                                          constant: -(baseHeight - currentY))
                if anchor.referenceGuide == .safeArea {
                    animationConstraint.constant += safeAreaInsets.bottom
                    targetY += safeAreaInsets.bottom

                }
            }
        case .bottom:
            switch referenceEdge(of: anchor) {
            case .top:
                animationConstraint = surfaceView.topAnchor.constraint(equalTo: layoutGuideProvider.topAnchor,
                                                                       constant: currentY)
                if anchor.referenceGuide == .safeArea {
                    animationConstraint.constant -= safeAreaInsets.top
                    targetY -= safeAreaInsets.top
                }
            case .bottom:
                let baseHeight = vc.view.bounds.height
                targetY = -(baseHeight - targetY)
                animationConstraint = surfaceView.topAnchor.constraint(equalTo: layoutGuideProvider.bottomAnchor,
                                                                       constant: -(baseHeight - currentY))
                if anchor.referenceGuide == .safeArea {
                    animationConstraint.constant += safeAreaInsets.bottom
                    targetY += safeAreaInsets.bottom

                }
            }
        }

        NSLayoutConstraint.activate([animationConstraint])
        self.animationEdgeConstraint = animationConstraint
        return (animationConstraint, targetY)
    }

    private func tearDownAnimationEdgeConstraint() {
        NSLayoutConstraint.deactivate(constraint: animationEdgeConstraint)
        animationEdgeConstraint = nil
    }

    // The method is separated from prepareLayout(to:) for the rotation support
    // It must be called in FloatingPanelController.traitCollectionDidChange(_:)
    func updateHeight() {
        guard let vc = vc else { return }
        NSLayoutConstraint.deactivate(constraint: heightConstraint)
        heightConstraint = nil

        if vc.contentMode == .fitToBounds {
            return
        }

        let anchor = layout.stateAnchors[self.edgeMostState]!
        if anchor is FloatingPanelIntrinsicLayoutAnchor {
            let heightMargin: CGFloat
            switch layout.anchorPosition {
            case .bottom:
                heightMargin = safeAreaInsets.bottom
            case .top:
                heightMargin = safeAreaInsets.top
            }
            let constant: CGFloat
            switch anchor.referenceGuide {
            case .safeArea:
                constant = surfaceView.intrinsicContentSize.height + heightMargin
            case .superview:
                constant = surfaceView.intrinsicContentSize.height
            }
            heightConstraint = surfaceView.heightAnchor.constraint(equalToConstant: constant)
        } else {
            switch layout.anchorPosition {
            case .top:
                heightConstraint = surfaceView.heightAnchor.constraint(equalToConstant: positionY(for: self.directionalMostState))
            case .bottom:
                heightConstraint = vc.view.heightAnchor.constraint(equalTo: surfaceView.heightAnchor, constant: positionY(for: self.directionalLeastState))
            }
        }
        NSLayoutConstraint.activate(constraint: heightConstraint)

        surfaceView.containerOverflow = vc.view.bounds.height
    }

    func updateInteractiveEdgeConstraint(diff: CGFloat, overflow: Bool, allowsRubberBanding: (UIRectEdge) -> Bool) {
        defer {
            log.debug("update surface location = \(surfaceEdgeLocation)")
        }

        let minConst: CGFloat = positionY(for: directionalLeastState)
        let maxConst: CGFloat = positionY(for: directionalMostState)

        var const = initialConst + diff

        // Rubberbanding top buffer
        if allowsRubberBanding(.top), const < minConst {
            let buffer = minConst - const
            const = minConst - rubberbandEffect(for: buffer, base: vc.view.bounds.height)
        }

        // Rubberbanding bottom buffer
        if allowsRubberBanding(.bottom), const > maxConst {
            let buffer = const - maxConst
            const = maxConst + rubberbandEffect(for: buffer, base: vc.view.bounds.height)
        }

        if overflow == false {
            const = min(max(const, minConst), maxConst)
        }

        interactionEdgeConstraint?.constant = const
    }

    // According to @chpwn's tweet: https://twitter.com/chpwn/status/285540192096497664
    // x = distance from the edge
    // c = constant value, UIScrollView uses 0.55
    // d = dimension, either width or height
    private func rubberbandEffect(for buffer: CGFloat, base: CGFloat) -> CGFloat {
        return (1.0 - (1.0 / ((buffer * 0.55 / base) + 1.0))) * base
    }

    func activateLayout(for state: FloatingPanelState, forceLayout: Bool = false) {
        defer {
            if forceLayout {
                layoutSurfaceIfNeeded()
                log.debug("activateLayout for \(state) -- surface.presentation = \(self.surfaceView.presentationFrame) surface.frame = \(self.surfaceView.frame)")
            } else {
                log.debug("activateLayout for \(state)")
            }
        }

        // Must deactivate `interactiveTopConstraint` here
        NSLayoutConstraint.deactivate(constraint: self.interactionEdgeConstraint)
        self.interactionEdgeConstraint = nil

        tearDownAnimationEdgeConstraint()

        NSLayoutConstraint.activate(fixedConstraints)

        if vc.contentMode == .fitToBounds {
            NSLayoutConstraint.activate(constraint: self.fitToBoundsConstraint)
        }

        var state = state

        setBackdropAlpha(of: state)

        if validStates.contains(state) == false {
            state = layout.initialState
        }

        NSLayoutConstraint.deactivate(fullConstraints + halfConstraints + tipConstraints + offConstraints)
        switch state {
        case .full:
            NSLayoutConstraint.activate(fullConstraints)
        case .half:
            NSLayoutConstraint.activate(halfConstraints)
        case .tip:
            NSLayoutConstraint.activate(tipConstraints)
        case .hidden:
            NSLayoutConstraint.activate(offConstraints)
        default:
            break
        }
    }

    private func layoutSurfaceIfNeeded() {
        #if !TEST
        guard surfaceView.window != nil else { return }
        #endif
        surfaceView.superview?.layoutIfNeeded()
    }

    private func setBackdropAlpha(of target: FloatingPanelState) {
        if target == .hidden {
            self.backdropView.alpha = 0.0
        } else {
            self.backdropView.alpha = backdropAlpha(for: target)
        }
    }

    func backdropAlpha(for state: FloatingPanelState) -> CGFloat {
        return layout.backdropAlpha?(for: state) ?? defaultLayout.backdropAlpha(for: state)
    }

    func checkLayout() {
        // Verify layout configurations
        assert(activeStates.count > 0)
        assert(validStates.contains(layout.initialState),
               "Does not include an initial state (\(layout.initialState)) in (\(validStates))")
        let statePosOrder = activeStates.sorted(by: { positionY(for: $0) < positionY(for: $1) })
        assert(sortedDirectionalStates == statePosOrder,
               "Check your layout anchors because the state order(\(statePosOrder)) must be (\(sortedDirectionalStates))).")
    }

    func segument(at pos: CGFloat, forward: Bool) -> LayoutSegment {
        /// ----------------------->Y
        /// --> forward                <-- backward
        /// |-------|===o===|-------|  |-------|-------|===o===|
        /// |-------|-------x=======|  |-------|=======x-------|
        /// |-------|-------|===o===|  |-------|===o===|-------|
        /// pos: o/x, seguement: =

        let sortedStates = sortedDirectionalStates

        let upperIndex: Int?
        if forward {
            upperIndex = sortedStates.firstIndex(where: { pos < positionY(for: $0) })
        } else {
            upperIndex = sortedStates.firstIndex(where: { pos <= positionY(for: $0) })
        }

        switch upperIndex {
        case 0:
            return LayoutSegment(lower: nil, upper: sortedStates.first)
        case let upperIndex?:
            return LayoutSegment(lower: sortedStates[upperIndex - 1], upper: sortedStates[upperIndex])
        default:
            return LayoutSegment(lower: sortedStates[sortedStates.endIndex - 1], upper: nil)
        }
    }
}
