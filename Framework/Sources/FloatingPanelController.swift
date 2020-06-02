//
//  Created by Shin Yamamoto on 2018/09/18.
//  Copyright © 2018 Shin Yamamoto. All rights reserved.
//

import UIKit

@objc public protocol FloatingPanelControllerDelegate: class {
    // Returns a FloatingPanelLayout object. If you use the default one, you can use a `FloatingPanelDefaultLayout` object.
    @objc(floatingPanel:layoutForTraitCollection:) optional
    func floatingPanel(_ fpc: FloatingPanelController, layoutFor newCollection: UITraitCollection) -> FloatingPanelLayout

    // Returns a FloatingPanelLayout object. If you use the default one, you can use a `FloatingPanelDefaultLayout` object.
    @objc(floatingPanel:layoutForSize:) optional
    func floatingPanel(_ fpc: FloatingPanelController, layoutFor size: CGSize) -> FloatingPanelLayout

    /// Returns a UIViewPropertyAnimator object to add/present a floating panel to a position.
    ///
    /// Default is the spring animator with 0.25 sec duration and `UIScrollView.DecelerationRate.fast` deceleration rate.
    @objc(floatingPanel:animatorForPresentingToState:) optional
    func floatingPanel(_ fpc: FloatingPanelController, animatorForPresentingTo state: FloatingPanelState) -> UIViewPropertyAnimator

    /// Returns a UIViewPropertyAnimator object to remove/dismiss a floating panel from a position.
    ///
    /// Default is the spring animator with 0.25 sec duration and `UIScrollView.DecelerationRate.fast` deceleration rate.
    @objc(floatingPanel:animatorForDismissingWithVelocity:) optional
    func floatingPanel(_ fpc: FloatingPanelController, animatorForDismissingWith velocity: CGVector) -> UIViewPropertyAnimator

    /// Called when the floating panel has changed to a new position. Can be called inside an animation block, so any
    /// view properties set inside this function will be automatically animated alongside the panel.
    @objc optional
    func floatingPanelDidChangePosition(_ fpc: FloatingPanelController)

    /// Asks the delegate if dragging should begin by the pan gesture recognizer.
    @objc optional
    func floatingPanelShouldBeginDragging(_ fpc: FloatingPanelController) -> Bool

    @objc optional
    func floatingPanelDidMove(_ fpc: FloatingPanelController) // any surface frame changes in dragging

    // called on start of dragging (may require some time and or distance to move)
    @objc optional
    func floatingPanelWillBeginDragging(_ fpc: FloatingPanelController)

    // called on finger up if the user dragged. velocity is in points/second.
    @objc optional
    func floatingPanelWillEndDragging(_ fpc: FloatingPanelController, withVelocity velocity: CGPoint, targetState: UnsafeMutablePointer<FloatingPanelState>)

    // called on finger up if the user dragged. decelerate is true if it will continue moving afterwards
    @objc optional
    func floatingPanelDidEndDragging(_ fpc: FloatingPanelController, willDecelerate decelerate: Bool)

    @objc optional
    func floatingPanelWillBeginDecelerating(_ fpc: FloatingPanelController, to state: FloatingPanelState) // called on finger up as we are moving
    @objc optional
    func floatingPanelDidEndDecelerating(_ fpc: FloatingPanelController) // called when scroll view grinds to a halt

    // TODO: Write doc comment
    @objc(floatingPanel:shouldRemoveAtLocation:withVelocity:)
    optional
    func floatingPanel(_ fpc: FloatingPanelController, shouldRemoveAt location: CGPoint, with velocity: CGVector) -> Bool

    // called on start of dragging to remove its views from a parent view controller
    @objc(floatingPanelWillRemove:withVelocity:)
    optional
    func floatingPanelWillRemove(_ fpc: FloatingPanelController, with velocity: CGPoint)

    // called when its views are removed from a parent view controller
    @objc optional
    func floatingPanelDidRemove(_ fpc: FloatingPanelController)

    /// Asks the delegate if the other gesture recognizer should be allowed to recognize the gesture in parallel.
    ///
    /// By default, any tap and long gesture recognizers are allowed to recognize gestures simultaneously.
    @objc(floatingPanel:shouldRecognizeSimultaneouslyWithGestureRecognizer:)
    optional
    func floatingPanel(_ fpc: FloatingPanelController, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool

    /// Asks the delegate for a content offset of the tracked scroll view to be pinned when a floating panel moves
    ///
    /// If you do not implement this method, the controller uses a value of the content offset plus the content insets
    /// of the tracked scroll view. Your implementation of this method can return a value for a navigation bar with a large
    /// title, for example.
    ///
    /// This method will not be called if the controller doesn't track any scroll view.
    @objc(floatingPanel:contentOffsetForPinningScrollView:)
    optional
    func floatingPanel(_ fpc: FloatingPanelController, contentOffsetForPinning trackingScrollView: UIScrollView) -> CGPoint
}


@objc
public class FloatingPanelState: NSObject, NSCopying, RawRepresentable {
    public typealias RawValue = String

    required public init?(rawValue: RawValue) {
        self.order = 0
        self.rawValue = rawValue
        super.init()
    }

    public init(rawValue: RawValue, order: Int) {
        self.rawValue = rawValue
        self.order = order
        super.init()
    }

    public let rawValue: RawValue
    public let order: Int
    
    public func copy(with zone: NSZone? = nil) -> Any {
        return self
    }

    public override var description: String {
        return rawValue
    }

    public override var debugDescription: String {
        return description
    }

    @objc(Full) public static let full: FloatingPanelState = FloatingPanelState(rawValue: "full", order: 1000)
    @objc(Half) public static let half: FloatingPanelState = FloatingPanelState(rawValue: "half", order: 500)
    @objc(Tip) public static let tip: FloatingPanelState = FloatingPanelState(rawValue: "tip", order: 100)
    @objc(Hidden) public static let hidden: FloatingPanelState = FloatingPanelState(rawValue: "hidden", order: 0)
}

extension FloatingPanelState {
    func next(in states: [FloatingPanelState]) -> FloatingPanelState {
        if let index = states.firstIndex(of: self), states.indices.contains(index + 1) {
            return states[index + 1]
        }
        return self
    }

    func pre(in states: [FloatingPanelState]) -> FloatingPanelState {
        if let index = states.firstIndex(of: self), states.indices.contains(index - 1) {
            return states[index - 1]
        }
        return self
    }
}

///
/// A container view controller to display a floating panel to present contents in parallel as a user wants.
///
@objc
open class FloatingPanelController: UIViewController {
    /// Constants indicating how safe area insets are added to the adjusted content inset.
    @objc
    public enum ContentInsetAdjustmentBehavior: Int {
        case always
        case never
    }

    /// A flag used to determine how the controller object lays out the content view when the surface position changes.
    @objc
    public enum ContentMode: Int {
        /// The option to fix the content to keep the height of the top most position.
        case `static`
        /// The option to scale the content to fit the bounds of the root view by changing the surface position.
        case fitToBounds
    }

    /// The delegate of the floating panel controller object.
    @objc 
    public weak var delegate: FloatingPanelControllerDelegate?{
        didSet{
            didUpdateDelegate()
        }
    }

    /// Returns the surface view managed by the controller object. It's the same as `self.view`.
    @objc
    public var surfaceView: FloatingPanelSurfaceView! {
        return floatingPanel.surfaceView
    }

    /// Returns the backdrop view managed by the controller object.
    @objc
    public var backdropView: FloatingPanelBackdropView! {
        return floatingPanel.backdropView
    }

    /// Returns the scroll view that the controller tracks.
    @objc
    public weak var scrollView: UIScrollView? {
        return floatingPanel.scrollView
    }

    // The underlying gesture recognizer for pan gestures
    @objc
    public var panGestureRecognizer: UIPanGestureRecognizer {
        return floatingPanel.panGestureRecognizer
    }

    /// The current position of the floating panel controller's contents.
    @objc
    public var state: FloatingPanelState {
        return floatingPanel.state
    }

    @objc
    public var isDecelerating: Bool {
        return floatingPanel.isDecelerating
    }

    /// The layout object managed by the controller
    @objc
    public var layout: FloatingPanelLayout {
        get { _layout }
        set {
            _layout = newValue
            if let parent = parent, let layout = newValue as? UIViewController, layout == parent {
                log.warning("A memory leak will occur by a retain cycle because \(self) owns the parent view controller(\(parent)) as the layout object. Don't let the parent adopt FloatingPanelLayout.")
            }
        }
    }

    /// The behavior object managed by the controller
    @objc
    public var behavior: FloatingPanelBehavior {
        get { _behavior }
        set {
            _behavior = newValue
            if let parent = parent, let behavior = newValue as? UIViewController, behavior == parent {
                log.warning("A memory leak will occur by a retain cycle because \(self) owns the parent view controller(\(parent)) as the behavior object. Don't let the parent adopt FloatingPanelBehavior.")
            }
        }
    }

    /// The content insets of the tracking scroll view derived from this safe area
    @objc
    public var adjustedContentInsets: UIEdgeInsets {
        return floatingPanel.layoutAdapter.adjustedContentInsets
    }

    /// The behavior for determining the adjusted content offsets.
    ///
    /// This property specifies how the content area of the tracking scroll view is modified using `adjustedContentInsets`. The default value of this property is FloatingPanelController.ContentInsetAdjustmentBehavior.always.
    @objc 
    public var contentInsetAdjustmentBehavior: ContentInsetAdjustmentBehavior = .always

    /// A Boolean value that determines whether the removal interaction is enabled.
    @objc
    public var isRemovalInteractionEnabled: Bool {
        @objc(setRemovalInteractionEnabled:) set { floatingPanel.isRemovalInteractionEnabled = newValue }
        @objc(isRemovalInteractionEnabled) get { return floatingPanel.isRemovalInteractionEnabled }
    }

    /// The view controller responsible for the content portion of the floating panel.
    @objc
    public var contentViewController: UIViewController? {
        set { set(contentViewController: newValue) }
        get { return _contentViewController }
    }

    /// The NearbyState determines that finger's nearby state.
    public var nearbyState: FloatingPanelState {
        let currentY = surfaceEdgeLocation.y
        return floatingPanel.targetPosition(from: currentY, with: .zero)
    }

    @objc
    public var contentMode: ContentMode = .static {
        didSet {
            guard state != .hidden else { return }
            activateLayout(forceLayout: false)
        }
    }

    private var _contentViewController: UIViewController?

    private(set) var floatingPanel: FloatingPanelCore!
    private var preSafeAreaInsets: UIEdgeInsets = .zero // Capture the latest one
    private var safeAreaInsetsObservation: NSKeyValueObservation?
    private let modalTransition = FloatingPanelModalTransition()

    required public init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        setUp()
    }

    override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
        super.init(nibName: nil, bundle: nil)
        setUp()
    }

    /// Initialize a newly created floating panel controller.
    @objc
    public init(delegate: FloatingPanelControllerDelegate? = nil) {
        super.init(nibName: nil, bundle: nil)
        self.delegate = delegate
        setUp()
    }

    private func setUp() {
        _ = FloatingPanelController.dismissSwizzling

        modalPresentationStyle = .custom
        transitioningDelegate = modalTransition

        let initialLayout: FloatingPanelLayout
        if let layout = delegate?.floatingPanel?(self, layoutFor: traitCollection) {
            initialLayout = layout
        } else {
            initialLayout = FloatingPanelBottomLayout()
        }
        let initialBehavior = FloatingPanelDefaultBehavior()

        floatingPanel = FloatingPanelCore(self, layout: initialLayout, behavior: initialBehavior)
    }

    private func didUpdateDelegate(){
        if let layout = delegate?.floatingPanel?(self, layoutFor: traitCollection) {
            _layout = layout
        }
    }

    // MARK:- Overrides

    /// Creates the view that the controller manages.
    open override func loadView() {
        assert(self.storyboard == nil, "Storyboard isn't supported")

        let view = FloatingPanelPassThroughView()
        view.backgroundColor = .clear

        backdropView.frame = view.bounds
        view.addSubview(backdropView)

        surfaceView.frame = view.bounds
        view.addSubview(surfaceView)

        self.view = view as UIView
    }

    open override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        if #available(iOS 11.0, *) {
            // Ensure the panel height after rotating a device
            floatingPanel.layoutAdapter.updateHeight()
        } else {
            // Because {top,bottom}LayoutGuide is managed as a view
            if floatingPanel.isDecelerating == false {
                self.update(safeAreaInsets: fp_safeAreaInsets)
            }
        }
        floatingPanel.layoutAdapter.checkLayout()
    }

    open override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
        super.viewWillTransition(to: size, with: coordinator)

        if self.view.bounds.size == size {
            return
        }

        // Change a layout for the new view size
        if let newLayout = self.delegate?.floatingPanel?(self, layoutFor: size) {
            layout = newLayout
            activateLayout(forceLayout: false)
        }

        if view.translatesAutoresizingMaskIntoConstraints {
            view.frame.size = size
            view.layoutIfNeeded()
        }
    }

    open override func willTransition(to newCollection: UITraitCollection, with coordinator: UIViewControllerTransitionCoordinator) {
        super.willTransition(to: newCollection, with: coordinator)

        if shouldUpdateLayout(from: traitCollection, to: newCollection) == false {
            return
        }

        // Change a layout for the new trait collection
        if let newLayout = self.delegate?.floatingPanel?(self, layoutFor: newCollection) {
            self.layout = newLayout
            activateLayout(forceLayout: false)
        }
    }

    open override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        safeAreaInsetsObservation = nil
    }

    // MARK:- Child view controller to consult
    open override var childForStatusBarStyle: UIViewController? {
        return contentViewController
    }

    open override var childForStatusBarHidden: UIViewController? {
        return contentViewController
    }

    open override var childForScreenEdgesDeferringSystemGestures: UIViewController? {
        return contentViewController
    }

    open override var childForHomeIndicatorAutoHidden: UIViewController? {
        return contentViewController
    }

    // MARK:- Privates

    private func shouldUpdateLayout(from previous: UITraitCollection, to new: UITraitCollection) -> Bool {
        return previous.horizontalSizeClass != new.horizontalSizeClass
            || previous.verticalSizeClass != new.verticalSizeClass
            || previous.preferredContentSizeCategory != new.preferredContentSizeCategory
            || previous.layoutDirection != new.layoutDirection
    }

    private func update(safeAreaInsets: UIEdgeInsets) {
        guard
            preSafeAreaInsets != safeAreaInsets
            else { return }

        log.debug("Update safeAreaInsets", safeAreaInsets)

        // Prevent an infinite loop on iOS 10: setUpLayout() -> viewDidLayoutSubviews() -> setUpLayout()
        preSafeAreaInsets = safeAreaInsets

        // preserve the current content offset if contentInsetAdjustmentBehavior is `.always`
        var contentOffset: CGPoint?
        if contentInsetAdjustmentBehavior == .always {
            contentOffset = scrollView?.contentOffset
        }

        floatingPanel.layoutAdapter.updateHeight()

        if let contentOffset = contentOffset {
            scrollView?.contentOffset = contentOffset
        }

        switch contentInsetAdjustmentBehavior {
        case .always:
            scrollView?.contentInset = adjustedContentInsets
        default:
            break
        }
    }

    private func activateLayout(forceLayout: Bool = false) {
        floatingPanel.layoutAdapter.prepareLayout()

        // preserve the current content offset if contentInsetAdjustmentBehavior is `.always`
        var contentOffset: CGPoint?
        if contentInsetAdjustmentBehavior == .always {
            contentOffset = scrollView?.contentOffset
        }

        floatingPanel.layoutAdapter.updateHeight()
        floatingPanel.layoutAdapter.activateLayout(for: floatingPanel.state, forceLayout: forceLayout)

        if let contentOffset = contentOffset {
            scrollView?.contentOffset = contentOffset
        }
    }

    // MARK: - Container view controller interface

    /// Shows the surface view at the initial position defined by the current layout
    @objc(show:completion:)
    public func show(animated: Bool = false, completion: (() -> Void)? = nil) {
        // Must apply the current layout here
        activateLayout(forceLayout: true)

        if #available(iOS 11.0, *) {
            // Must track the safeAreaInsets of `self.view` to update the layout.
            // There are 2 reasons.
            // 1. This or the parent VC doesn't call viewSafeAreaInsetsDidChange() on the bottom
            // inset's update expectedly.
            // 2. The safe area top inset can be variable on the large title navigation bar(iOS11+).
            // That's why it needs the observation to keep `adjustedContentInsets` correct.
            safeAreaInsetsObservation = self.view.observe(\.safeAreaInsets, options: [.initial, .new, .old]) { [weak self] (_, change) in
                // Use `self.view.safeAreaInsets` becauese `change.newValue` can be nil in particular case when
                // is reported in https://github.com/SCENEE/FloatingPanel/issues/330
                guard let self = self, change.oldValue != self.view.safeAreaInsets else { return }
                self.update(safeAreaInsets: self.view.safeAreaInsets)
            }
        } else {
            // KVOs for topLayoutGuide & bottomLayoutGuide are not effective.
            // Instead, update(safeAreaInsets:) is called at `viewDidLayoutSubviews()`
        }

        move(to: floatingPanel.layoutAdapter.initialState,
             animated: animated,
             completion: completion)
    }

    /// Hides the surface view to the hidden position
    @objc(hide:completion:)
    public func hide(animated: Bool = false, completion: (() -> Void)? = nil) {
        move(to: .hidden,
             animated: animated,
             completion: completion)
    }

    /// Adds the view managed by the controller as a child of the specified view controller.
    /// - Parameters:
    ///     - parent: A parent view controller object that displays FloatingPanelController's view. A container view controller object isn't applicable.
    ///     - viewIndex: Insert the surface view managed by the controller below the specified view index. By default, the surface view will be added to the end of the parent list of subviews.
    ///     - animated: Pass true to animate the presentation; otherwise, pass false.
    @objc(addPanelToParent:at:animated:)
    public func addPanel(toParent parent: UIViewController, at viewIndex: Int = -1, animated: Bool = false) {
        guard self.parent == nil else {
            log.warning("Already added to a parent(\(parent))")
            return
        }
        assert((parent is UINavigationController) == false, "UINavigationController displays only one child view controller at a time.")
        assert((parent is UITabBarController) == false, "UITabBarController displays child view controllers with a radio-style selection interface")
        assert((parent is UISplitViewController) == false, "UISplitViewController manages two child view controllers in a master-detail interface")
        assert((parent is UITableViewController) == false, "UITableViewController should not be the parent because the view is a table view so that a floating panel doens't work well")
        assert((parent is UICollectionViewController) == false, "UICollectionViewController should not be the parent because the view is a collection view so that a floating panel doens't work well")

        if viewIndex < 0 {
            parent.view.addSubview(self.view)
        } else {
            parent.view.insertSubview(self.view, at: viewIndex)
        }

        parent.addChild(self)

        view.frame = parent.view.bounds // Needed for a correct safe area configuration
        view.translatesAutoresizingMaskIntoConstraints = false
        NSLayoutConstraint.activate([
            self.view.topAnchor.constraint(equalTo: parent.view.topAnchor, constant: 0.0),
            self.view.leftAnchor.constraint(equalTo: parent.view.leftAnchor, constant: 0.0),
            self.view.rightAnchor.constraint(equalTo: parent.view.rightAnchor, constant: 0.0),
            self.view.bottomAnchor.constraint(equalTo: parent.view.bottomAnchor, constant: 0.0),
            ])

        show(animated: animated) { [weak self] in
            guard let `self` = self else { return }
            self.didMove(toParent: parent)
        }
    }

    /// Removes the controller and the managed view from its parent view controller
    /// - Parameters:
    ///     - animated: Pass true to animate the presentation; otherwise, pass false.
    ///     - completion: The block to execute after the view controller is dismissed. This block has no return value and takes no parameters. You may specify nil for this parameter.
    @objc(removePanelFromParent:completion:)
    public func removePanelFromParent(animated: Bool, completion: (() -> Void)? = nil) {
        guard self.parent != nil else {
            completion?()
            return
        }

        delegate?.floatingPanelWillRemove?(self, with: .zero)

        hide(animated: animated) { [weak self] in
            guard let `self` = self else { return }

            self.willMove(toParent: nil)

            self.view.removeFromSuperview()

            self.removeFromParent()

            self.delegate?.floatingPanelDidRemove?(self)
            completion?()
        }
    }

    /// Moves the position to the specified position.
    /// - Parameters:
    ///     - to: Pass a FloatingPanelPosition value to move the surface view to the position.
    ///     - animated: Pass true to animate the presentation; otherwise, pass false.
    ///     - completion: The block to execute after the view controller has finished moving. This block has no return value and takes no parameters. You may specify nil for this parameter.
    @objc(moveToState:animated:completion:)
    public func move(to: FloatingPanelState, animated: Bool, completion: (() -> Void)? = nil) {
        assert(floatingPanel.layoutAdapter.vc != nil, "Use show(animated:completion)")
        floatingPanel.move(to: to, animated: animated, completion: completion)
    }

    /// Sets the view controller responsible for the content portion of the floating panel.
    public func set(contentViewController: UIViewController?) {
        if let vc = _contentViewController {
            vc.willMove(toParent: nil)
            vc.view.removeFromSuperview()
            vc.removeFromParent()
        }

        if let vc = contentViewController {
            addChild(vc)

            let surfaceView = floatingPanel.surfaceView
            surfaceView.set(contentView: vc.view)

            vc.didMove(toParent: self)
        }

        _contentViewController = contentViewController
    }

    // MARK: - Scroll view tracking

    /// Tracks the specified scroll view to correspond with the scroll.
    ///
    /// - Parameters:
    ///     - scrollView: Specify a scroll view to continuously and seamlessly work in concert with interactions of the surface view or nil to cancel it.
    @objc(trackScrollView:)
    public func track(scrollView: UIScrollView?) {
        guard let scrollView = scrollView else {
            floatingPanel.scrollView = nil
            return
        }

        floatingPanel.scrollView = scrollView

        switch contentInsetAdjustmentBehavior {
        case .always:
            if #available(iOS 11.0, *) {
                scrollView.contentInsetAdjustmentBehavior = .never
            } else {
                children.forEach { (vc) in
                    vc.automaticallyAdjustsScrollViewInsets = false
                }
            }
        default:
            break
        }
    }

    // MARK: - Utilities

    /// Updates the layout object from the delegate and lays out the views managed
    /// by the controller immediately.
    ///
    /// This method updates the `FloatingPanelLayout` object from the delegate and
    /// then it calls `layoutIfNeeded()` of the root view to force the view
    /// to update the floating panel's layout immediately. It can be called in an
    /// animation block.
    @objc
    public func invalidateLayout() {
        activateLayout(forceLayout: true)
    }

    /// Returns the y-coordinate of the point at the origin of the surface view.
    @objc
    public func surfaceEdgeLocation(for state: FloatingPanelState) -> CGPoint {
        return floatingPanel.layoutAdapter.surfaceEdgeLocation(for: state)
    }

    @objc
    public var surfaceEdgeLocation: CGPoint {
        get { floatingPanel.layoutAdapter.surfaceEdgeLocation }
        set { floatingPanel.layoutAdapter.surfaceEdgeLocation = newValue }
    }
}

extension FloatingPanelController {
    private static let dismissSwizzling: Any? = {
        let aClass: AnyClass! = UIViewController.self //object_getClass(vc)
        if let imp = class_getMethodImplementation(aClass, #selector(dismiss(animated:completion:))),
            let originalAltMethod = class_getInstanceMethod(aClass, #selector(fp_original_dismiss(animated:completion:))) {
            method_setImplementation(originalAltMethod, imp)
        }
        let originalMethod = class_getInstanceMethod(aClass, #selector(dismiss(animated:completion:)))
        let swizzledMethod = class_getInstanceMethod(aClass, #selector(fp_dismiss(animated:completion:)))
        if let originalMethod = originalMethod, let swizzledMethod = swizzledMethod {
            // switch implementation..
            method_exchangeImplementations(originalMethod, swizzledMethod)
        }
        return nil
    }()
}

public extension UIViewController {
    @objc func fp_original_dismiss(animated flag: Bool, completion: (() -> Void)? = nil) {
        // Implementation will be replaced by IMP of self.dismiss(animated:completion:)
    }
    @objc func fp_dismiss(animated flag: Bool, completion: (() -> Void)? = nil) {
        // Call dismiss(animated:completion:) to a content view controller
        if let fpc = parent as? FloatingPanelController {
            if fpc.presentingViewController != nil {
                self.fp_original_dismiss(animated: flag, completion: completion)
            } else {
                fpc.removePanelFromParent(animated: flag, completion: completion)
            }
            return
        }
        // Call dismiss(animated:completion:) to FloatingPanelController directly
        if let fpc = self as? FloatingPanelController {
            // When a panel is presented modally and it's not a child view controller of the presented view controller.
            if fpc.presentingViewController != nil, fpc.parent == nil {
                self.fp_original_dismiss(animated: flag, completion: completion)
            } else {
                fpc.removePanelFromParent(animated: flag, completion: completion)
            }
            return
        }

        // For other view controllers
        self.fp_original_dismiss(animated: flag, completion: completion)
    }
}
