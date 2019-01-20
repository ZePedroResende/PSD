package directory.health;

import com.codahale.metrics.health.HealthCheck;
import com.google.common.base.Optional;
import directory.core.Template;

public class TemplateHealthCheck extends HealthCheck {
    private final Template template;

    public TemplateHealthCheck(Template template) {
        this.template = template;
    }

    @Override
    protected Result check() throws Exception {
        template.render(Optional.of("woo"));
        template.render(Optional.absent());
        return Result.healthy();
    }
}