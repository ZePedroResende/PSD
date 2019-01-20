package directory;

import directory.core.Template;
import io.dropwizard.Configuration;
import org.hibernate.validator.constraints.NotEmpty;

public class DirectoryConfiguration extends Configuration {
    @NotEmpty
    private String template;

    @NotEmpty
    private String defaultName = "peer-lending";

    public String getTemplate() {
        return template;
    }

    public void setTemplate(String template) {
        this.template = template;
    }

    public Template buildTemplate() {
        return new Template(template, defaultName);
    }

    public String getDefaultName() {
        return defaultName;
    }

    public void setDefaultName(String name) {
        this.defaultName = name;
    }
}