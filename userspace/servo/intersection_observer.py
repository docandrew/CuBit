"""Pinned Servo observer lifecycle repair, independently of API enablement."""


def apply(edit):
    edit(
        "components/script/dom/intersectionobserver/intersectionobserver.rs",
        """    fn disconnect_from_owner(&self) {
        if self.connected_to_document.get() {
            self.owner_doc.remove_intersection_observer(self);
        }
    }""",
        """    fn disconnect_from_owner(&self) {
        // A later observe() must re-register this observer with the document.
        // Clear membership when removing it, including after the last unobserve.
        if self.connected_to_document.replace(false) {
            self.owner_doc.remove_intersection_observer(self);
        }
    }""",
        1,
    )
    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """        // > When calculating the root intersection rectangle for a same-origin-domain target,
        // > the rectangle is then expanded according to the offsets in the IntersectionObserver’s
        // > [[rootMargin]] slot in a manner similar to CSS’s margin property, with the four values
        // > indicating the amount the top, right, bottom, and left edges, respectively, are offset by,
        // > with positive lengths indicating an outward offset. Percentages are resolved relative to
        // > the width of the undilated rectangle.
        // TODO(stevennovaryo): add check for same-origin-domain
        intersection_rectangle.map(|intersection_rectangle| {
            let margin = Self::resolve_percentages_with_basis(
                &self.root_margin.borrow(),
                intersection_rectangle,
            );
            intersection_rectangle.outer_rect(margin)
        })
    }
""",
        """        // Margins depend on each target's origin; apply them during target computation.
        intersection_rectangle
    }

    fn is_same_origin_domain_target(&self, target: &Element) -> bool {
        let root_document = match self.concrete_root() {
            Some(ElementOrDocument::Element(element)) => element.owner_document(),
            Some(ElementOrDocument::Document(document)) => document,
            // A non-local top document must not expose geometry or margins.
            None => return false,
        };
        root_document.origin().same_origin_domain(&target.owner_document().origin())
    }
""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '        let (Some(root_bounds), Some(target_rect), Some(root_intersection)) =',
        '        let (Some(mut root_bounds), Some(target_rect), Some(root_intersection)) =',
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '        // TODO(stevennovaryo): we should probably also consider adding visibity check, ideally',
        """        let same_origin_domain = self.is_same_origin_domain_target(target);
        if same_origin_domain {
            root_bounds = root_bounds.outer_rect(Self::resolve_percentages_with_basis(
                &self.root_margin.borrow(), root_bounds,
            ));
        }

        // TODO(stevennovaryo): we should probably also consider adding visibity check, ideally""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """            &self.scroll_margin.borrow(),
        );""",
        """            same_origin_domain.then_some(&*self.scroll_margin.borrow()),
        );""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """    scroll_margin: &IntersectionObserverMargin,
) -> Option<Rect<Au, CSSPixel>>""",
        """    scroll_margin: Option<&IntersectionObserverMargin>,
) -> Option<Rect<Au, CSSPixel>>""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """                if containing_element.establishes_scroll_container_without_reflow() {
                    let margin = IntersectionObserver::resolve_percentages_with_basis(""",
        """                if containing_element.establishes_scroll_container_without_reflow() &&
                    let Some(scroll_margin) = scroll_margin
                {
                    let margin = IntersectionObserver::resolve_percentages_with_basis(""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """    fn queue_an_intersectionobserverentry(
        &self,
        cx: &mut JSContext,
        document: &Document,
        time: CrossProcessInstant,
        root_bounds: Rect<Au, CSSPixel>,""",
        """    fn queue_an_intersectionobserverentry(
        &self,
        cx: &mut JSContext,
        document: &Document,
        time: CrossProcessInstant,
        root_bounds: Option<Rect<Au, CSSPixel>>,""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '        let root_bounds = rect_to_domrectreadonly(root_bounds);',
        '        let root_bounds = root_bounds.map(&mut rect_to_domrectreadonly);',
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '            Some(&root_bounds),',
        '            root_bounds.as_deref(),',
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """                // TODO(stevennovaryo): Per IntersectionObserverEntry interface, the rootBounds
                //                      should be null for cross-origin-domain target.""",
        '                // Suppress root geometry for cross-origin-domain targets.',
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        """                    intersection_output.root_bounds,
                    intersection_output.target_rect,""",
        """                    self.is_same_origin_domain_target(target)
                        .then_some(intersection_output.root_bounds),
                    intersection_output.target_rect,""",
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '            inner.0.to_used_value(containing_block.height()),',
        '            inner.0.to_used_value(containing_block.width()),',
        1,
    )

    edit(
        'components/script/dom/intersectionobserver/intersectionobserver.rs',
        '            inner.2.to_used_value(containing_block.height()),',
        '            inner.2.to_used_value(containing_block.width()),',
        1,
    )

