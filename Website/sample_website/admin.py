from django.contrib import admin
from .models import Book

admin.site.register(Book)
class BookAdmin(admin.ModelAdmin):
    list_display = ('title', 'pdf_file', 'cover_image')