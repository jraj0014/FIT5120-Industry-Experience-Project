"""
URL configuration for sample_website project.

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/5.0/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.urls import path
from django.views.generic.base import TemplateView
from django.urls import path, re_path
from django.contrib.auth import views as auth_views
from django.conf import settings
from django.conf.urls.static import static
from . import views
from .views import book_list

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', views.password_entry, name='password_entry'),
    path('home/', views.index, name='index'),
    # path('', views.index, name ="index"),
    path('about', views.about, name ="about"),
    path('test', views.test, name ="test"),
    path('contact', views.contact, name ="contact"), # views.index becuase views has a fucntin index
    path('read-pdf/', views.read_pdf, name='read_pdf_view'),
    path('end-session/', views.index, name='end_session_view'),
    path('send-email/', views.send_email, name='send_email'),
    path('books/', book_list, name='book_list'),
    path('reports/', views.reports, name='reports'),
    # Django Auth
    path('accounts/login',auth_views.LoginView.as_view(template_name='accounts/login.html'), name='login'),
    path('accounts/logout',auth_views.LogoutView.as_view(template_name='accounts/logout.html'), name='logout'),
]   + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)